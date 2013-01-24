{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
import Shelly
import Control.Arrow ((***))
import Control.Monad
import Data.Char as C
import Data.List as L
import Data.Maybe
import Prelude hiding (FilePath)
import Data.Text.Lazy as LT
default (LT.Text)

list, list' :: [(Char, Char)]
list' =
    [ ('ě', 'e')
    , ('š', 's')
    , ('č', 'c')
    , ('ř', 'r')
    , ('ž', 'z')
    , ('ý', 'y')
    , ('á', 'a')
    , ('í', 'i')
    , ('é', 'e')
    , ('ť', 't')
    , ('ů', 'u')
    , ('ú', 'u')
    , ('ó', 'o')
    , ('ď', 'd')
    , ('ň', 'n')
    ]
list = list' ++ fmap (C.toUpper *** C.toUpper) list'

newName :: FilePath -> FilePath
newName = fromText . LT.pack
        . L.reverse
        . handleFileExtension
        . avoidSpaces
        . fmap replace
        . LT.unpack . toTextIgnore
    where
        replace :: Char -> Char
        replace c = fromMaybe c $ L.lookup c list

        handleFileExtension :: String -> String
        handleFileExtension = uncurry (++)
                            . (fix *** id)
                            . L.span (/= '.')
            where
                fix s | L.length s > 4 = s
                      | otherwise      = fmap C.toLower s

        -- returns filenames reversed
        avoidSpaces :: String -> String
        avoidSpaces = snd . L.foldl avoidSpace (False, "")

        avoidSpace :: (Bool, String) -> Char -> (Bool, String)
        avoidSpace (b,t) c
            | c == ' '  = (True, t)
            | otherwise = (False, (if b then C.toUpper c else c):t)

showPaths :: FilePath -> FilePath -> Sh ()
showPaths p1 p2 =
    echo $ LT.concat [toTextIgnore p1, LT.pack " -> ", toTextIgnore p2]

rename :: (FilePath -> FilePath -> Sh ()) -> FilePath -> Sh ()
rename exec fp = do
    let newFp = newName fp

    when (newFp /= fp) $ do
        fileExists <- test_f newFp
        if not fileExists
            then exec fp newFp
            else echo $ LT.pack "file exists: " `append` toTextIgnore fp

-- TODO nefunguje spravne na podslozkach, pokud se prejmenujeslozka
--        "a hop" -> "aHop"
--      potom neprojde prejmenovani souboru
--        "a hop/zelva.JPG" -> "aHop/zelva.jpg"
--
-- K obejiti tohoto problemu staci spustit prejmenovavani vicekrat za sebou

main :: IO ()
main = shelly $ verbosely $ do
    liftIO $ putStr "Current PATH: "
    echo . toTextIgnore =<< pwd

    ts <- Shelly.find "."
    mapM_ (rename showPaths) ts  -- testovani
    -- mapM_ (rename mv) ts  -- prejmenovani

