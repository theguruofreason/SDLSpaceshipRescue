{-# LANGUAGE TemplateHaskell #-}
module Entities where
import           Control.Lens
import           Items
import Graphics.Gloss

data EntityType = Fauna { _healthCurrent :: Int
                        , _healthMax     :: Int
                        , _size          :: Int
                        , _eHead         :: String
                        , _body1         :: String
                        , _body2         :: Maybe String
                        , _legs          :: String
                        , _wings         :: String
                        , _tail          :: Maybe String
                        , _weapon1       :: Item
                        , _weapon2       :: Maybe Item
                        , _armor         :: Item
                        , _defense       :: Int
                        , _movement      :: Int --ticks per move
                        , _inventory     :: Inventory
                        }
                |
                  Flora { }


data Entity = Entity { _name                :: String
                     , _sprite              :: Picture
                     , _type                :: EntityType
                     , _lowTierHarvestItems :: Inventory
                     , _midTierHarvestItems :: Inventory
                     , _topTierHarvestItems :: Inventory
                     , _growTime            :: Int
                     , _harvestDifficulty   :: Int
                     }

$(makeLenses ''Entity)
$(makeLenses ''EntityType)
