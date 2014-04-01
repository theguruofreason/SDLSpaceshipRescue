{-# LANGUAGE TemplateHaskell #-}
module CreatureGen where
import Control.Lens
import qualified Data.Map as MAP
import qualified Text.JSON as JSON
import Entities
import System.Random

generateNPCCreature :: Int -> Entity
generateNPCCreature difficultyValue = do
  startingGen <- newStdGen


generateName :: StdGen -> (StdGen, String)
generateName generator = do
  pref <- prefixes !! (getStdRandom randomR (0, length prefixes))
          where
            prefixes = ["an", "ad", "ala", "bi", "bor", "bak", "cer", "chi", "clu", "cro", "div", "da", "do", "des", "dru", "ero", "eb", "ei", "eto", "fia", "fi", "fla", "fes", "gro", "gai", "ge", "gli", "hi", "hun", "his", "her", "ilu", "iro", "ia", "igni", "ju", "jus", "jib", "ja", "ki", "ka", "kit", "ker", "li", "lo", "lus" "lig", "mon", "ma", "mer", "mai", "
