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
import Graphics.Gloss.Interface.Pure.Game
import           Items
import           System.FilePath
import Data.Map

data World = World { _characterPosX     :: Float
                   , _characterPosY     :: Float
                   , _characterSprite   :: Picture
                   , _sprites           :: Picture
                   }
$(makeLenses ''World)

tileTranslate tiles = 12 * tiles

defaultWorld :: Picture -> World
defaultWorld image = World { _characterPosX = tileTranslate 5
                           , _characterPosY = tileTranslate 5
                           , _characterSprite = circleSolid 20
                           , _sprites = image
                           }

drawWorld :: World -> Picture
drawWorld world = translate (world^.characterPosX) (world^.characterPosY) (world^.sprites)


main :: IO ()
main = do
  (RGBA arr) <- readRepaImage ("." </> "resources" </> "Nice_curses_12x12.png")
  let
      window = InWindow "test 1" (800, 640) (0, 0)
      (offset, size) = (shapeOfList [4,12,180], shapeOfList [4, 12, 12])
      (w,h,charSprite) = imageToPicture True . RGBA . computeS $ extract offset size arr
  play window black 60 (defaultWorld (scale 2 3 charSprite)) drawWorld keyHandler (flip const)

keymap :: Map (Key,KeyState) (World -> World)
keymap = fromList [ ((SpecialKey KeyUp, Down), characterMove 0 1 )
                  , ((SpecialKey KeyDown, Down), characterMove 0 (-1))
                  , ((SpecialKey KeyLeft, Down), characterMove (-1) 0)
                  , ((SpecialKey KeyRight, Down),  characterMove 1 0)
                  ]
    where
      characterMove xMove yMove world= world { _characterPosX = world^.characterPosX + tileTranslate xMove
                                              , _characterPosY = world^.characterPosY + tileTranslate yMove
                                              }
  
keyHandler :: Event -> World -> World
keyHandler (EventKey k ks _ _) = findWithDefault id (k, ks) keymap
