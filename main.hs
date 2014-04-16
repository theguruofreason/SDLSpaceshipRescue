import           Data.Array.Repa
import           Data.Array.Repa.IO.DevIL
import           Data.Array.Repa.Repr.ForeignPtr
import           Data.Array.Repa.Shape
import           Data.Maybe
import           Entities
import           Foreign.ForeignPtr
import qualified Graphics.UI.SDL                 as SDL
import           Items
import           System.FilePath

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
         window <- SDL.trySetVideoMode 640 480 24 [SDL.HWSurface]
         checkSuccess window
         keyRepeatSuccess <- SDL.enableKeyRepeat 500 100
         spriteSheetSurface <- loadImageToSurface ("." </> "resources" </> "Belal_Smooth_Walls.png")
         let
             spriteRect = SDL.Rect 16 16 16 16
             positionRect = SDL.Rect 320 240 16 16
         mainLoop (fromJust window) spriteSheetSurface spriteRect positionRect
         SDL.quit

-- imageToSurface image =

loadImageToSurface :: FilePath -> IO SDL.Surface
loadImageToSurface path = do
  theImagePath <- runIL $ readImage path
  makeSurface theImagePath
    where
      makeSurface (RGBA arr) = quickMake arr
      makeSurface (RGB arr) = quickMake arr
      makeSurface (BGRA arr) = quickMake arr
      makeSurface (BGR arr) = quickMake arr
      makeSurface (Grey arr) = quickMake arr
      quickMake a = withForeignPtr (toForeignPtr a) $ (\p -> SDL.createRGBSurfaceFrom p (width a) (height a) 1 1 0x000000FF 0x0000FF00 0x00FF0000 0xFF000000)
      width theArr = head $ listOfShape . extent $ theArr
      height theArr = head . reverse $ listOfShape . extent $ theArr

checkSuccess :: Maybe a -> IO ()
checkSuccess Nothing = do
  theError <- SDL.getError
  SDL.failWithError (fromJust theError)
checkSuccess (Just _) = return ()

defaultRect :: SDL.Rect
defaultRect = SDL.Rect 320 240 10 10

mainLoop :: SDL.Surface -> SDL.Surface -> SDL.Rect -> SDL.Rect -> IO ()
mainLoop surface spriteSheet sprite position = do
  ticks <- SDL.getTicks
  putStrLn $ show ticks
  SDL.updateRect surface position
  SDL.fillRect surface Nothing (SDL.Pixel 0x000000)
  SDL.blitSurface spriteSheet (Just sprite) surface (Just position)
  SDL.flip surface
  event <- SDL.waitEvent
  case event of
    SDL.KeyDown k -> keyHandler (SDL.symKey k) surface spriteSheet sprite position
    SDL.MouseButtonDown _ _ _ -> return ()
    _ -> mainLoop surface spriteSheet sprite position

keyHandler :: SDL.SDLKey -> SDL.Surface -> SDL.Surface -> SDL.Rect -> SDL.Rect -> IO ()
keyHandler k window spriteSheet sprite position =
    case k of
      SDL.SDLK_UP -> mainLoop window spriteSheet sprite $ changeableRectangle position $ Just UP
      SDL.SDLK_DOWN  -> mainLoop window spriteSheet sprite $ changeableRectangle position $ Just DOWN
      SDL.SDLK_LEFT -> mainLoop window spriteSheet sprite $ changeableRectangle position $ Just LEFT
      SDL.SDLK_RIGHT -> mainLoop window spriteSheet sprite $ changeableRectangle position $ Just RIGHT
      _ -> return ()

data Direction = UP | DOWN | LEFT | RIGHT

changeableRectangle :: SDL.Rect -> Maybe Direction -> SDL.Rect
changeableRectangle oldRect direction =
    case direction of
      Just UP -> changeableRectangle (oldRect {SDL.rectY = (SDL.rectY oldRect) - 16}) Nothing
      Just DOWN -> changeableRectangle (oldRect {SDL.rectY = (SDL.rectY oldRect) + 16}) Nothing
      Just LEFT -> changeableRectangle (oldRect {SDL.rectX = (SDL.rectX oldRect) - 16}) Nothing
      Just RIGHT -> changeableRectangle (oldRect {SDL.rectX = (SDL.rectX oldRect) +16}) Nothing
      Nothing -> oldRect
