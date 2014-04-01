{-# LANGUAGE TemplateHaskell #-}
module CreatureGen where
import Control.Lens
import qualified Data.Map as MAP
import qualified Text.JSON as JSON
import Entities
import System.Random

--generateNPCCreature :: Int -> Entity
--generateNPCCreature difficultyValue = do
--  startingGen <- newStdGen

generatePrefix :: StdGen -> (String, StdGen)
generatePrefix generator = case fst $ useThird gen3 of
                             True -> (first : second : third : [], gen3)
                             False -> (first : second : [], gen3)
    where
      (first, gen1) = (randomR ('a','z') generator)
      (second, gen2) = case first `elem` vowels of
                        True -> (consonants !! fst (randomR (0, length consonants - 1) gen1), mkStdGen $ fst (random gen1 :: (Int, StdGen)))
                        False -> (vowels !! fst (randomR (0, length vowels - 1) gen1), mkStdGen $ fst (random gen1 :: (Int, StdGen)))
      (third, gen3) = (letters !! (fst (randomR (0, length ['a','b'..'z'] - 1) gen2)), mkStdGen $ fst (random gen2 :: (Int, StdGen)))
      letters = ['a','b'..'z']
      vowels = ['a','e','i','o','u','y']
      consonants = filter (not . (`elem` vowels)) letters
      useThird gen = (random gen) :: (Bool, StdGen)

--generateName :: StdGen -> IO (String, StdGen)
--generateName generator = do
--  prefix <- generatePrefix generator
--  return (prefix, generator)


--            prefixes = ["an", "ad", "ala", "bi", "bor", "bak", "cer", "chi", "clu", "cro", "div", "da", "do", "des", "dru", "ero", "eb", "ei", "eto", "fia", "fi", "fla", "fes", "gro", "gai", "ge", "gli", "hi", "hun", "his", "her", "ilu", "iro", "ia", "igni", "ju", "jus", "jib", "ja", "ki", "ka", "kit", "ker", "li", "lo", "lus" "lig", "mon", "ma", "mer", "mai"]

                       
