{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  ( main,
  )
where

import Data.ByteString qualified as BS
import Data.Char
import Data.Function
import Data.Map.Strict qualified as M
import GHC.SysTools.Ar
import GHC.Utils.TmpFs
import System.Directory
import System.FilePath
import System.Process
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup "GHC.SysTools.Ar out-of-tree tests" $
      [ localOption (QuickCheckTests 100000) $ testProperty name $ \ar ->
          ioProperty $ do
            ar' <- roundtripSimple writer ar
            pure $ ar == ar'
      | (name, writer) <-
          [("writeBSDAr", writeBSDAr), ("writeGNUAr", writeGNUAr)]
      ]
        -- macOS ar is BSD ar, in which case this needs to be changed
        -- to use writeBSDAr and ensure the archive is not empty.
        --
        -- TODO: llvm-ar doesn't work with either
        -- writeBSDAr/writeGNUAr
        ++ [ localOption (QuickCheckTests 1000) $
               testProperty "writeGNUAr/ar" $
                 \ar -> ioProperty $ do
                   ar' <- roundtripViaAR writeGNUAr "ar" ar
                   pure $ ar == ar'
           ]

-- | Write an 'Archive' via 'writeBSDAr' or 'writeGNUAr', then load it
-- back via 'loadAr'.
roundtripSimple :: (FilePath -> Archive -> IO ()) -> Archive -> IO Archive
roundtripSimple writer ar = do
  tmp <- getTemporaryDirectory
  withTempDirectory tmp "ghc-ar-quickcheck" $ \dir -> do
    let f = dir </> "test.a"
    writer f ar
    canonicalizeArchive <$> loadAr f

-- | Write an 'Archive' via 'writeBSDAr' or 'writeGNUAr', extract it
-- and re-archive it using an external ar program, then load it back
-- via 'loadAr'.
roundtripViaAR ::
  (FilePath -> Archive -> IO ()) -> FilePath -> Archive -> IO Archive
roundtripViaAR writer ar_prog ar@(Archive objs) = do
  tmp <- getTemporaryDirectory
  withTempDirectory tmp "ghc-ar-quickcheck" $ \dir -> do
    let f = dir </> "test.a"
    writer f ar
    _ <- callProcessIn ar_prog ["x", f] dir
    removeFile f
    _ <- callProcessIn ar_prog (["qc", f] <> fmap filename objs) dir
    canonicalizeArchive <$> loadAr f

callProcessIn :: FilePath -> [String] -> FilePath -> IO ()
callProcessIn prog args cwd = do
  _ <- readCreateProcess ((proc prog args) {cwd = Just cwd}) ""
  pure ()

-- | Canonicalize an 'Archive' to:
--
-- * Ensure file names are unique, so ar x wouldn't overwrite an
--   object file
-- * Sort objects by file names and mask irrelevant info, so the 'Eq'
--   instance of 'Archive' works
canonicalizeArchive :: Archive -> Archive
canonicalizeArchive (Archive objs) =
  Archive . M.elems . M.fromList $ f <$> objs
  where
    f obj =
      ( filename obj,
        obj {filetime = 0, fileown = 0, filegrp = 0, filemode = 644}
      )

instance Arbitrary Archive where
  arbitrary = canonicalizeArchive . Archive <$> arbitrary

  shrink (Archive objs) = canonicalizeArchive . Archive <$> shrink objs

instance Arbitrary ArchiveEntry where
  arbitrary = do
    -- Threshold of short filename stored in ar_name in-place:
    --
    -- * BSD variant: up to 16 bytes
    -- * SVR4/GNU variant: up to 15 bytes
    --
    -- This should cover both short/long cases with roughly equal
    -- probability.
    filename_len <- chooseInt (10, 20)
    filename <- resize filename_len genFileName
    filedata <- arbitrary
    pure $
      ArchiveEntry
        { filename,
          filetime = 0,
          fileown = 0,
          filegrp = 0,
          filemode = 644,
          filesize = BS.length filedata,
          filedata
        }

  shrink obj =
    [ obj {filename = f, filesize = BS.length buf, filedata = buf}
    | (f, buf) <- shrink (filename obj, filedata obj),
      validFileName f
    ]

-- | Simplest way of generating a valid filename on all platforms, no
-- need to consider special characters, case sensitivity or forbidden
-- win32 names like CON etc
genFileName :: Gen FilePath
genFileName = sized $ \n -> vectorOf n $ chooseEnum ('0', '9')

validFileName :: FilePath -> Bool
validFileName "" = False
validFileName fn = all isDigit fn
