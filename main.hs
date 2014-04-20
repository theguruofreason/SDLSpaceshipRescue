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

tileTranslate tiles = 12 * tiles
 
defaultWorld :: Picture -> World
defaultWorld image = World { _characterPosition = (tileTranslate 5, tileTranslate 5)
                           , _characterSprite = circleSolid 20
                           , _sprites = image
                           }

drawWorld :: World -> Picture
drawWorld world = translate (world^.characterPosition_1) (world^.characterPosition_2) world^.sprites


main :: IO ()
main = do
  (RGBA arr) <- readRepaImage ("." </> "resources" </> "Nice_curses_12x12.png")
  let
      window = InWindow "test 1" (800, 640) (0, 0)
      (offset, size) = (shapeOfList [4,12,180], shapeOfList [4, 12, 12])
      (w,h,charSprite) = imageToPicture True . RGBA . computeS $ extract offset size arr
  play window black 60 (defaultWorld (scale 2 3 charSprite)) drawWorld keyHandler (flip const)

keyHandler key world =
    case key of
      KeyUp Down -> world { _characterPosition = (world^.characterPosition_1, world^.characterPosition_2 + tileTranslate 1)
      KeyDown Down -> world { _characterPosition = (world^.characterPosition_1, world^.characterPosition_2 1 tileTranslate 1)
      KeyLeft Down -> world { _characterPosition = (world^.characterPosition_1 - tileTranslate 1, world^.characterPosition_2)
      KeyRight Down -> world { _characterPosition = (world^.characterPosition_1 + tileTranslate 1, world^.characterPosition_2)
