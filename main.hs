{-# LANGUAGE TemplateHaskell #-}
import           Control.Lens
import           Data.Array.Repa
import           Data.Array.Repa.IO.DevIL
import           Data.Array.Repa.Repr.ForeignPtr
import           Data.Array.Repa.Shape
import           Data.Maybe
import           Entities
import           Foreign.ForeignPtr
import           Graphics.Gloss
import           Graphics.Gloss.DevIL
import           Items
import           System.FilePath

data World = World { _characterPosition :: (Int, Int)
                   , _characterSprite   :: Picture
                   , _sprites           :: Picture
                   }
$(makeLenses ''World)

defaultWorld :: Picture -> World
defaultWorld image = World { _characterPosition = (10,10)
                           , _characterSprite = circleSolid 20
                           , _sprites = image
                           }

drawWorld :: World -> Picture
drawWorld world = world^.sprites


main :: IO ()
main = do
  (RGBA arr) <- readRepaImage ("." </> "resources" </> "Nice_curses_12x12.png")
  print . listOfShape . extent $ arr
  let
      window = InWindow "test 1" (800, 640) (0, 0)
      (offset, size) = (shapeOfList [4,12,180], shapeOfList [4, 12, 12])
      (w,h,pic) = imageToPicture True . RGBA . computeS $ extract offset size arr
      {-charSprite = imageToPicture $ computeS $ extract (fst charShape) (snd charShape) spriteSheet-}
  play window black 60 (defaultWorld (scale 2 3 pic)) drawWorld (flip const) (flip const)

