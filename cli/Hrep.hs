module Main (main) where

import Control.Applicative     ((<**>))
import Control.Monad           (filterM)
import Control.Monad.IO.Class  (liftIO)
import Data.Foldable           (for_)
import Data.List.NonEmpty      (NonEmpty (..))
import Data.Machine            ((<~))
import Data.Semigroup.Foldable (foldMap1)
import Language.Haskell.Lexer  (Pos (..), Token, lexerPass0, rmSpace)
import System.Directory
import System.FilePath         (takeExtension, (</>))

import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Machine         as M
import qualified Options.Applicative  as O

-------------------------------------------------------------------------------
-- File system
-------------------------------------------------------------------------------

files :: [FilePath] -> M.ProcessT IO i FilePath
files fps = recursiveFiles (const True) (\fp -> takeExtension fp == ".hs") <~ M.source fps

recursiveFiles
    :: (FilePath -> Bool) -- ^ directory predicate
    -> (FilePath -> Bool) -- ^ files predicate
    -> M.ProcessT IO FilePath FilePath
recursiveFiles pd pf = M.MachineT . return $ M.Await (\root -> f [root]) M.Refl M.stopped where
    f :: [FilePath] -> M.ProcessT IO FilePath FilePath
    f []     = recursiveFiles pd pf
    f (x:xs) = M.MachineT $ do
        dirOrFile <- dirOrFile x
        case dirOrFile of
            IsDir | pd x -> do
                contents <- map (x </>) <$> listDirectory x
                M.runMachineT $ f (contents ++ xs)
            IsFile | pf x -> do
                return $ M.Yield x $ f xs
            otherwise -> M.runMachineT $ f xs

data DirOrFile = IsDir | IsFile | IsOther

dirOrFile :: FilePath -> IO DirOrFile
dirOrFile fp = do
    isDir <- doesDirectoryExist fp
    if isDir
    then return IsDir
    else do
        isFile <- doesFileExist fp
        if isFile
        then return IsFile
        else return IsOther

stringTokens :: String -> [(Pos, String)]
stringTokens = map snd . rmSpace . lexerPass0

-------------------------------------------------------------------------------
-- Matching
-------------------------------------------------------------------------------

match :: [String] -> [(Pos, String)] -> [[Pos]]
match []     = const []
match needle = go
  where
    needleN = length needle

    go []                 = []
    go tokens@(_:tokens')
        | match' needle (map snd h) = map fst h : go t
        | otherwise                 = go tokens'
      where
        (h , t) = splitAt needleN tokens

match' :: [String] -> [String] -> Bool
match' []       []     = True
match' xs       []     = False
match' []       ys     = False
match' ("_":xs) (_:ys) = match' xs ys
match' (x:xs)   (y:ys) = x == y && match' xs ys

data MinMax a = MinMax a a

instance Ord a => Semigroup (MinMax a) where
    MinMax x y <> MinMax x' y' = MinMax (min x x') (max y y')

printLines :: FilePath -> Int -> Int -> [String] -> IO ()
printLines fp mi ma xs = for_ zs $ \(n, z) ->
    putStrLn $ fp ++ ":" ++ show n ++ ":" ++ z
  where
    ys = zip [1..] xs
    zs = takeWhile ((<= ma) . fst) . dropWhile ((< mi) . fst) $ ys

-------------------------------------------------------------------------------
-- Hrep
-------------------------------------------------------------------------------

hrep :: [String] ->  M.ProcessT IO FilePath o
hrep needle = M.repeatedly $ do
    fp <- M.await
    contentsBS <- liftIO $ BS.readFile fp
    let contents = UTF8.toString contentsBS
    let tokens   = stringTokens contents
    let poss     = match needle tokens

    liftIO $ for_ poss $ \poss' -> case poss' of
        []     -> return ()
        (p:ps) -> do
            let MinMax l1 l2 = foldMap1 (\(Pos _ l _) -> MinMax l l) (p :| ps)
            printLines fp l1 l2 (lines contents)

-------------------------------------------------------------------------------
-- Opts
-------------------------------------------------------------------------------

-- TODO: add other options
data Opts = Opts
    { optsNeedle :: [String]
    , optsFiles  :: [FilePath]
    }

optsP :: O.Parser Opts
optsP = Opts
    <$> (map snd . stringTokens <$> O.strArgument (O.metavar "PATTERN"))
    <*> O.many (O.strArgument (O.metavar "FILES"))

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    opts <- O.execParser (O.info (optsP <**> O.helper) infos)
    M.runT_ $ hrep (optsNeedle opts) <~ files (optsFiles opts)
  where
    infos = mconcat
        [ O.fullDesc
        , O.header "hrep - lexically grep Haskell source code"
        ]
