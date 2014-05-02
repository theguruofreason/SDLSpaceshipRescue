{-# LANGUAGE TemplateHaskell #-}
import           Control.Lens
import           Data.Array.Repa
import           Data.Array.Repa.IO.DevIL
import           Data.Array.Repa.Repr.ForeignPtr
import           Data.Array.Repa.Shape
import           Data.Map
import           Data.Maybe
import Debug.Trace
import           Entities
import           Foreign.ForeignPtr
import           Graphics.Gloss
import           Graphics.Gloss.DevIL
import           Graphics.Gloss.Interface.Pure.Game
import           System.FilePath

data World = World { _characterPosX   :: Float
                   , _characterPosY   :: Float
                   , _characterSprite :: Picture
                   , _sprites         :: Picture
                   }
$(makeLenses ''World)

tileScale tiles = 24 * tiles

defaultWorld :: Picture -> World
defaultWorld image = World { _characterPosX = tileScale 5
                           , _characterPosY = tileScale 5
                           , _characterSprite = circleSolid 20
                           , _sprites = image
                           }

drawWorld :: World -> Picture
drawWorld world = translate (world^.characterPosX) (world^.characterPosY) (world^.sprites)


main :: IO ()
main = do
  (RGBA spriteSheet) <- readRepaImage ("." </> "resources" </> "Nice_curses_12x12.png")
  let
      window = InWindow "test 1" (800, 640) (0, 0)
      (offset, size) = (shapeOfList [4,12,180], shapeOfList [4, 12, 12])
      (w,h,charSprite) = imageToPicture True . RGBA . computeS $ extract offset size spriteSheet
  play window black 60 (defaultWorld (scale 2 2 charSprite)) drawWorld keyHandler (flip const)

keymap :: Map (Key,KeyState) (World -> World)
keymap = fromList [ ((SpecialKey KeyUp, Down), characterMove 0 1 )
                  , ((SpecialKey KeyDown, Down), characterMove 0 (-1))
                  , ((SpecialKey KeyLeft, Down), characterMove (-1) 0)
                  , ((SpecialKey KeyRight, Down),  characterMove 1 0)
                  , ((SpecialKey KeyPad1, Down), characterMove (-1) (-1))
                  , ((SpecialKey KeyPad2, Down), characterMove 0 (-1))
                  , ((SpecialKey KeyPad3, Down), characterMove 1 (-1))
                  , ((SpecialKey KeyPad4, Down), characterMove (-1) 0)
                  , ((SpecialKey KeyPad5, Down), characterMove 0 0)
                  , ((SpecialKey KeyPad6, Down), characterMove 1 0)
                  , ((SpecialKey KeyPad7, Down), characterMove (-1) 1)
                  , ((SpecialKey KeyPad8, Down), characterMove 0 1)
                  , ((SpecialKey KeyPad9, Down), characterMove 1 1)
                  ]
    where
      characterMove xMove yMove world = world { _characterPosX = world^.characterPosX + tileScale xMove
                                              , _characterPosY = world^.characterPosY + tileScale yMove
                                              }

keyHandler :: Event -> World -> World
keyHandler (EventKey k ks _ _) = trace (show k) $ findWithDefault id (k, ks) keymap
keyHandler (EventMotion _) = id
keyHandler (EventResize _) = id
