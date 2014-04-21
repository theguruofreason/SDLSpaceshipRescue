{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
module WorldMap where
import Control.Lens
import System.FilePath
import Data.Map
import qualified Data.Vector as V
import Graphics.Gloss
import Entities
import Items
import System.Random
import Data.Array.Repa

data Tile = forall sh. Shape sh => Tile { _displayName :: String
                                        , _groundSprite :: (sh, sh)
                                        , _creatures :: [Entity]
                                        , _itemsOnTile :: Inventory
                                        }
$(makeLenses ''Tile)

data TerrainTiles = TerrainTiles { _dirt :: Tile
                                 , _snow :: Tile
                                 , _sand :: Tile
                                 , _mud :: Tile
                                 , _rock :: Tile
                                 , _water :: Tile
                                 , _acid :: Tile
                                 }


defaultTerrainTiles =
    TerrainTiles { _dirt = Tile { _displayName = "dirt"
                                , _groundSprite = (shapeOfList [4, 84, 156], shapeOfList [4, 12, 12])
                                , _creatures = []
                                , _itemsOnTile = []
                                }
                 , _snow = Tile { _displayName = "snow"
                                , _groundSprite = (shapeOfList [4, 132, 24], shapeOfList [4, 12, 12])
                                , _creatures = []
                                , _itemsOnTile = []
                                }
                 , _sand = Tile { _displayName = "sand"
                                , _groundSprite = (shapeOfList [4, 12, 144], shapeOfList [4, 12, 12])
                                , _creatures = []
                                , _itemsOnTile = []
                                }
                 , _mud = Tile { _displayName = "mud"
                                , _groundSprite = (shapeOfList [4, 24, 48], shapeOfList [4, 12, 12])
                                , _creatures = []
                                , _itemsOnTile = []
                                }
                 , _rock = Tile { _displayName = "rock"
                                , _groundSprite = (shapeOfList [4, 120, 156], shapeOfList [4, 12, 12])
                                , _creatures = []
                                , _itemsOnTile = []
                                }
                 , _water = Tile { _displayName = "water"
                                , _groundSprite = (shapeOfList [4, 84, 0], shapeOfList [4, 12, 12])
                                , _creatures = []
                                , _itemsOnTile = []
                                }
                 , _acid = Tile { _displayName = "acid"
                                , _groundSprite = (shapeOfList [4, 144, 12], shapeOfList [4, 12, 12])
                                , _creatures = []
                                , _itemsOnTile = []
                                }
                 }
 
type TileMap = Map (Int, Int) Tile

data Biome = Plain | Grassland | Swamp | AcidSwamp | Desert | Rocky | Mountainous | Barren | Tundra

--tileChunk :: Biome -> TileMap
{-tileChunk biome = do
  return $ case biome of
             Plain -> zip (zip [1..12] [1..12]) (
-}
