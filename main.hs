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

main :: IO ()
main = do
  spriteSheet <- loadDevILPicture ("." </> "resources" </> "Belal_Smooth_Walls.png")
  let
      window = InWindow "test 1" (640, 480) (10, 10)
  mainLoop spriteSheet


data SpriteSheet = SpriteSheet Int Int Picture

data World = World { _characterPosition :: (Int, Int)
                   , _characterSprite :: Picture
                   , _spriteSheet :: SpriteSheet
                   }

defaultworld spriteSheet = World { _characterPosition = (10,10)
                     , _characterSprite = 
