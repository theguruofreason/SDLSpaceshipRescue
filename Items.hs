{-# LANGUAGE TemplateHaskell #-}
module Items where
import           Control.Lens


data ItemInventory = Inventory | NoInventory
                   deriving (Read, Eq)
                            
type Inventory = [ItemSlot]

data ItemSlot = ItemSlot { _letter :: Char -- for interaction
                         , _item   :: Item
                         , _amount :: Int -- number of items currently
                                          -- in the stack
                         }

data ItemType = WeaponRanged | WeaponMelee | Food | Armor | Container
              deriving (Read, Eq)
                       
data Item = Item { _name                   :: String
                 , _itemType               :: ItemType
                 , _description            :: String
                 , _mass                   :: Int -- in kgs
                 , _volume                 :: Int -- in m^3
                 , _attackRating           :: Int
                 , _attackRange            :: Int
                 , _ammoCount              :: (Int, Int)
                 , _reloadSpeed            :: Int
                 , _nutritionalValue       :: Int
                 , _armorValue             :: Int
                 , _containerStorageVolume :: (Int, Int) -- in use, max
                 , _itemInventory          :: ItemInventory
                 , _specialProperties      :: [ItemProperties]
                 }
          deriving (Read, Eq)

data ItemProperties = LightSource | StunGun | Bulletproof | AllowsStealth | Burns | Corrodes
                    deriving (Read, Eq)
                             
$(makeLenses ''Item)
$(makeLenses ''ItemSlot)
