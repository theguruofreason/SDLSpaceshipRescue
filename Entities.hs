{-# LANGUAGE TemplateHaskell #-}
module Entities where
import           Control.Lens
import           Items

type Fraction = (Int,Int)

data Entity = Entity { _name                    :: String
                     , _health                  :: Fraction
                     , _size :: Int
                     , _head                    :: String
                     , _body1                   :: String
                     , _body2                   :: Maybe String
                     , _legs                    :: String
                     , _wings                   :: Maybe String
                     , _tail                    :: Maybe String
                     , _weapon1                 :: Item -- 2 weapon slots
                     , _weapon2                 :: Item
                     , _armor                   :: Item
                     , _sizeModifier            :: Float
                     , _defense                 :: Int
                     , _inventory               :: Inventory
                     }

$(makeLenses ''Entity)
