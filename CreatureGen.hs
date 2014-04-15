{-# LANGUAGE TemplateHaskell #-}
module CreatureGen where
import           Control.Lens
import           Data.List     (zip4, zipWith4)
import qualified Data.Map      as MAP
import           Entities
import           System.Random
import qualified Text.JSON     as JSON

--generateNPCCreature :: Int -> Entity
--generateNPCCreature difficultyValue = do
--  startingGen <- newStdGen

bases :: [String]
bases = ["saur", "gliatop", "grat", "felix", "panth", "canin", "canid", "big", "incat", "zephyr", "rex", "gnat"]

generatePrefixes = do
  let
      randomLetters = newStdGen >>= return . randomRs ('a','z')
  allLetters1 <- randomLetters
  allLetters2 <- randomLetters
  let
      vowels = filter (`elem` "aeiouy") allLetters1
      consonants = filter (not . (`elem` "aeiouy")) allLetters1
      determinePrefix (firstLetter, vowel, consonant, anyLetter) = if firstLetter `elem` "aeiouy" then
                                                                  firstLetter : consonant : anyLetter : []
                                                              else
                                                                  firstLetter : vowel : anyLetter: []
  return $ map determinePrefix $ zip4 allLetters1 vowels consonants allLetters2

generateSuffixes = do
  let
      randomLetters = newStdGen >>= return . randomRs ('a','z')
  allLetters1 <- randomLetters
  let
      vowels = filter (`elem` "aeiouy") allLetters1
      consonants = filter (not . (`elem` "aeiouy")) allLetters1
      determineSuffix (firstLetter, vowel, consonant) = if firstLetter `elem` "aeiouy" then
                                                            firstLetter : consonant : []
                                                        else
                                                            firstLetter : vowel : []
  return $ map determineSuffix $ zip3 allLetters1 vowels consonants



{-letters, vowels, consonants :: [Char]
letters = ['a','b'..'z']
vowels = ['a','e','i','o','u','y']
consonants = filter (not . (`elem` vowels)) letters

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
      useThird gen = (random gen) :: (Bool, StdGen)

generateBaseOfName :: (String, StdGen) -> (String, StdGen)
generateBaseOfName (prefix, generator) = case last prefix `elem` vowels of
                                           True -> generateBaseFromVowel prefix generator
                                           False -> generateBaseFromConsonant prefix generator
    where
      generateBaseFromVowel pref gen = (pref ++ (fst $ pickBase gen), snd $ pickBase gen)
      generateBaseFromConsonant pref gen = (pref ++ generateVowel gen : (fst $ pickBase gen), snd $ pickBase gen)
      pickBase gen = (bases !! fst (randomR (0, length bases - 1) gen), mkStdGen $ fst (random gen :: (Int, StdGen)))
      generateVowel gen = vowels !! fst (randomR (0, length vowels - 1) gen)


generateName :: StdGen -> IO (String, StdGen)
generateName generator = do
  prefix <- generatePrefix generator
  return (prefix, generator)
-}
