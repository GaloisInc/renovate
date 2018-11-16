{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
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

analysis :: R.AnalyzeOnly arch binFmt (Const Int)
analysis = R.AnalyzeOnly myAnalysis

analysisConfigs :: [(R.Architecture, R.SomeConfig R.AnalyzeOnly (Const Int))]
analysisConfigs = [ (R.PPC32, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RP.config32 analysis))
                  , (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RP.config64 analysis))
                  , (R.X86_64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RX.config analysis))
                  ]

myAnalyzeElf :: E.SomeElf E.Elf -> IO Int
myAnalyzeElf someElf = do
  fha <- FH.newHandleAllocator
  R.withElfConfig someElf analysisConfigs $ \config e loadedBinary -> do
    (res, diags) <- R.analyzeElf config fha e loadedBinary
    print diags
    return (getConst res)
