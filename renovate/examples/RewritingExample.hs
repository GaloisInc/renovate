{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
import           Control.Lens ( (^.) )
import qualified Data.ByteString as BS
import           Data.Functor.Const ( Const(..) )
import qualified Data.ElfEdit as E                   -- (elf-edit)
import qualified Data.Macaw.BinaryLoader as MBL      -- (macaw-loader)
import           Data.Macaw.BinaryLoader.X86 ()      -- (macaw-loader-x86)
import qualified Data.Parameterized.NatRepr as NR    -- (parameterized-utils)
import qualified Lang.Crucible.FunctionHandle as FH  -- (crucible)
import qualified Renovate as R                       -- (renovate)
import qualified Renovate.Arch.PPC as RP             -- (renovate-ppc)
import qualified Renovate.Arch.X86_64 as RX          -- (renovate-x86)

-- | The pre-analysis phase allows callers to perform setup actions in the
-- 'R.RewriteM' monad.  This can be useful for e.g., allocating fresh global
-- variables that need to be referenced in the analysis phase.  The pre-analysis
-- has access to the analysis environment.
myPreAnalysis :: (R.HasAnalysisEnv env) => env arch binFmt -> R.RewriteM arch (Const () arch)
myPreAnalysis _ = return (Const ())

-- | The analysis phase takes place in the IO monad (enabling things like
-- logging and sophisticated solver invocations).  It has access to the analysis
-- environment and the results from the pre-analysis phase (unused in this
-- example).
myAnalysis :: (R.HasAnalysisEnv env) => env arch binFmt -> Const () arch -> IO (Const Int arch)
myAnalysis env _ = do
  let bs = R.biBlocks (R.analysisBlockInfo env)
  return (Const (length bs))

-- | A fresh type to hold state for the rewriter.
newtype RewriteState arch = RewriteState (R.SymbolicAddress arch)

-- | The pre-rewriting phase runs in the 'R.RewriteM' monad and nhas access to
-- the analysis results.  It can also be useful for allocating fresh global
-- variables.
myPreRewriter :: (R.HasAnalysisEnv env) => env arch binFmt -> Const Int arch -> R.RewriteM arch (RewriteState arch)
myPreRewriter env nBlocks = do
  let fn = BS.pack (replicate (getConst nBlocks) 0xF4)
  addr <- R.injectFunction "rawData" fn
  return (RewriteState addr)

-- | The rewriter runs in the 'R.RewriteM' monad and has access to the analysis
-- environment and the results of both the analysis and the pre-rewrite phases.
--
-- The rewriter transforms basic blocks.
myRewriter :: (R.HasAnalysisEnv env)
           => env arch binFmt
           -> Const Int arch
           -> RewriteState arch
           -> R.SymbolicBlock arch
           -> R.RewriteM arch (Maybe [R.TaggedInstruction arch (R.InstructionAnnotation arch)])
myRewriter env nBlocks (RewriteState newFuncAddr) symBlock =
  return (Just (R.basicBlockInstructions symBlock))

analysis :: R.AnalyzeAndRewrite arch binFmt (Const Int)
analysis = R.AnalyzeAndRewrite { R.arPreAnalyze = myPreAnalysis
                               , R.arAnalyze = myAnalysis
                               , R.arPreRewrite = myPreRewriter
                               , R.arRewrite = myRewriter
                               }

analysisConfigs :: [(R.Architecture, R.SomeConfig R.AnalyzeAndRewrite (Const Int))]
analysisConfigs = [ (R.PPC32, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RP.config32 analysis))
                  , (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RP.config64 analysis))
                  , (R.X86_64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RX.config analysis))
                  ]

myAnalyzeElf :: E.SomeElf E.Elf -> IO Int
myAnalyzeElf someElf = do
  fha <- FH.newHandleAllocator
  R.withElfConfig someElf analysisConfigs $ \config e loadedBinary -> do
    (newElf, res, ri) <- R.rewriteElf config fha e loadedBinary R.Parallel
    print (getConst res)
    print (ri ^. R.riBlockMapping)
    return (getConst res)
