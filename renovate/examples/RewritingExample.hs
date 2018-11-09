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

myAnalysis :: (R.HasAnalysisEnv env) => env arch binFmt -> IO (Const Int arch)
myAnalysis env = do
  let bs = R.biBlocks (R.analysisBlockInfo env)
  return (Const (length bs))

newtype RewriteState arch = RewriteState (R.SymbolicAddress arch)

myInit :: (R.HasAnalysisEnv env) => env arch binFmt -> Const Int arch -> R.RewriteM arch (RewriteState arch)
myInit env nBlocks = do
  let fn = BS.pack (replicate (getConst nBlocks) 0xF4)
  addr <- R.injectFunction "rawData" fn
  return (RewriteState addr)

myRewriter :: (R.HasAnalysisEnv env)
           => env arch binFmt
           -> Const Int arch
           -> RewriteState arch
           -> R.SymbolicBlock arch
           -> R.RewriteM arch (Maybe [R.TaggedInstruction arch (R.InstructionAnnotation arch)])
myRewriter env nBlocks (RewriteState newFuncAddr) symBlock =
  return (Just (R.basicBlockInstructions symBlock))

analysis :: R.AnalyzeAndRewrite arch binFmt (Const Int)
analysis = R.AnalyzeAndRewrite { R.arAnalyze = myAnalysis
                               , R.arInitializeRewriter = myInit
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
