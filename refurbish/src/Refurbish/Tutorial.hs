
{-|

This module contains examples of writing both a standalone analysis pass and a
combined analysis and rewriting pass using renovate.  It attempts to show all of
the steps necessary for invoking the library to rewrite an ELF file for three
architectures: PowerPC32, PowerPC64, and X86_64.  These examples are in the
repository under the @renovate\/renovate\/examples@ directory.

A typical analysis looks something like:

>>> :set -XDataKinds
>>> :set -XTypeApplications
>>> import           Data.Functor.Const ( Const(..) )
>>> import qualified Data.ElfEdit as E                   -- (elf-edit)
>>> import qualified Data.Macaw.BinaryLoader as MBL      -- (macaw-loader)
>>> import           Data.Macaw.BinaryLoader.X86 ()      -- (macaw-loader-x86)
>>> import qualified Data.Parameterized.NatRepr as NR    -- (parameterized-utils)
>>> import qualified Lang.Crucible.FunctionHandle as FH  -- (crucible)
>>> import qualified Renovate as R                       -- (renovate)
>>> import qualified Renovate.Arch.PPC as RP             -- (renovate-ppc)
>>> import qualified Renovate.Arch.X86_64 as RX          -- (renovate-x86)
>>> import           Data.Macaw.X86.Symbolic ()
>>> :{
myAnalysis :: (R.HasAnalysisEnv env) => env arch binFmt -> IO (Const Int arch)
myAnalysis env = do
  let bs = R.biBlocks (R.analysisBlockInfo env)
  return (Const (length bs))
:}

>>> :{
analysis :: R.AnalyzeOnly arch binFmt (Const Int)
analysis = R.AnalyzeOnly myAnalysis
:}

>>> :{
analysisConfigs :: [(R.Architecture, R.SomeConfig R.AnalyzeOnly (Const Int))]
analysisConfigs = [ (R.PPC32, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RP.config32 analysis))
                  , (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RP.config64 analysis))
                  , (R.X86_64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RX.config analysis))
                  ]
:}

>>> :{
myAnalyzeElf :: E.SomeElf E.Elf -> IO Int
myAnalyzeElf someElf = do
  fha <- FH.newHandleAllocator
  R.withElfConfig someElf analysisConfigs $ \config e loadedBinary -> do
    (res, diags) <- R.analyzeElf config fha e loadedBinary
    print diags
    return (getConst res)
:}

Note that the analysis function (@myAnalysis@ in the example) has access to
all of the data from the 'HasAnalysisEnv` class, including the ISA, ABI,
binary image (see 'MBL.LoadedBinary'), and recovered basic blocks from the
input binary.

Note that the @analysisConfigs@ have type @'R.SomeConfig' 'R.AnalyzeOnly' x@,
which means that they are only usable with 'R.analyzeElf'.

An example rewriter looks something like:

>>> :set -XDataKinds
>>> :set -XTypeApplications
>>> import           Control.Lens ( (^.) )
>>> import qualified Data.ByteString as BS
>>> import           Data.Functor.Const ( Const(..) )
>>> import qualified Data.ElfEdit as E                   -- (elf-edit)
>>> import qualified Data.Macaw.BinaryLoader as MBL      -- (macaw-loader)
>>> import           Data.Macaw.BinaryLoader.X86 ()      -- (macaw-loader-x86)
>>> import qualified Data.Parameterized.NatRepr as NR    -- (parameterized-utils)
>>> import qualified Lang.Crucible.FunctionHandle as FH  -- (crucible)
>>> import qualified Renovate as R                       -- (renovate)
>>> import qualified Renovate.Arch.PPC as RP             -- (renovate-ppc)
>>> import qualified Renovate.Arch.X86_64 as RX          -- (renovate-x86)
>>>
>>> :{
-- | The pre-analysis phase allows callers to perform setup actions in the
-- 'R.RewriteM' monad.  This can be useful for e.g., allocating fresh global
-- variables that need to be referenced in the analysis phase.  The pre-analysis
-- has access to the analysis environment.
myPreAnalysis :: (R.HasAnalysisEnv env) => env arch binFmt -> R.RewriteM lm arch (Const () arch)
myPreAnalysis _ = return (Const ())
:}

>>> :{
-- | The analysis phase takes place in the IO monad (enabling things like
-- logging and sophisticated solver invocations).  It has access to the analysis
-- environment and the results from the pre-analysis phase (unused in this
-- example).
myAnalysis :: (R.HasAnalysisEnv env) => env arch binFmt -> Const () arch -> IO (Const Int arch)
myAnalysis env _ = do
  let bs = R.biBlocks (R.analysisBlockInfo env)
  return (Const (length bs))
:}

>>> :{
-- | A fresh type to hold state for the rewriter.
newtype RewriteState arch = RewriteState (R.SymbolicAddress arch)
:}

>>> :{
-- | The pre-rewriting phase runs in the 'R.RewriteM' monad and has access to
-- the analysis results.  It can also be useful for allocating fresh global
-- variables.
myPreRewriter :: (R.HasAnalysisEnv env) => env arch binFmt -> Const Int arch -> R.RewriteM lm arch (RewriteState arch)
myPreRewriter env nBlocks = do
  let fn = BS.pack (replicate (getConst nBlocks) 0xF4)
  addr <- R.injectFunction "rawData" fn
  return (RewriteState addr)
:}

>>> :{
-- | The rewriter runs in the 'R.RewriteM' monad and has access to the analysis
-- environment and the results of both the analysis and the pre-rewrite phases.
--
-- The rewriter transforms basic blocks.
myRewriter :: (R.HasAnalysisEnv env)
           => env arch binFmt
           -> Const Int arch
           -> RewriteState arch
           -> R.SymbolicBlock arch
           -> R.RewriteM lm arch (Maybe [R.TaggedInstruction arch (R.InstructionAnnotation arch)])
myRewriter env nBlocks (RewriteState newFuncAddr) symBlock =
  return (Just (R.basicBlockInstructions symBlock))
:}

>>> :{
analysis :: R.AnalyzeAndRewrite lm arch binFmt (Const Int)
analysis = R.AnalyzeAndRewrite { R.arPreAnalyze = myPreAnalysis
                               , R.arAnalyze = myAnalysis
                               , R.arPreRewrite = myPreRewriter
                               , R.arRewrite = myRewriter
                               }
:}

>>> :{
analysisConfigs :: [(R.Architecture, R.SomeConfig (R.AnalyzeAndRewrite lm) (Const Int))]
analysisConfigs = [ (R.PPC32, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RP.config32 analysis))
                  , (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RP.config64 analysis))
                  , (R.X86_64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RX.config analysis))
                  ]
:}

>>> :{
myAnalyzeElf :: E.SomeElf E.Elf -> IO Int
myAnalyzeElf someElf = do
  fha <- FH.newHandleAllocator
  R.withElfConfig someElf analysisConfigs $ \config e loadedBinary -> do
    let strat = R.LayoutStrategy R.Parallel R.BlockGrouping R.AlwaysTrampoline
    (newElf, res, ri) <- R.rewriteElf config fha e loadedBinary strat
    print (getConst res)
    print (ri ^. R.riBlockMapping)
    return (getConst res)
:}


The result of the analysis pass (@myAnalysis@ in the example) feeds into the
rewriter initializer (@myInit@ in the example), whose purpose is to allocate
global data (in the 'RW.RewriteM' monad) for use during the rewriting pass.
The actual rewriting pass (@myRewriter@ in the example), which modifies one
block at a time.  Note that the rewriter can return 'Nothing' for a block,
which tells renovate to not modify the block at all.
-}
module Refurbish.Tutorial () where
