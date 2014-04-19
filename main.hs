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
  mainLoop spriteSheet


data SpriteSheet = SpriteSheet Int Int Picture

