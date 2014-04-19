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
  (_, _, spriteSheet) <- loadDevILPicture ("." </> "resources" </> "Belal_Smooth_Walls.png")
  let
      window = InWindow "test 1" (640, 480) (10, 10)
  play window black 60 (defaultWorld spriteSheet) drawWorld (flip const) (flip const)
