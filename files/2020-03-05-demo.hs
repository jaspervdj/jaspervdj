{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Demo where

import Visual

import Data.List
import Data.Char
import Control.Monad (join)
import Data.Traversable (for)
import Data.Tuple
import qualified Data.Vector.Storable as V
import Control.Monad.ST (runST)
import qualified Data.Vector.Storable.Mutable as VM
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Codec.Picture.Png as JP

$(industryStandardBoilerPlate (Just '━') '(━►) Nothing)
$(industryStandardBoilerPlate (Just '━') '(┭►) (Just '─'))
$(industryStandardBoilerPlate (Just '━') '(┓) Nothing)
$(industryStandardBoilerPlate (Just '─') '(┶►) (Just '━'))
$(industryStandardBoilerPlate (Just '━') '(╋►) (Just '━'))
$(industryStandardBoilerPlate (Just '━') '(┳►) (Just '━'))

example02 =
 (📈) (partition isUpper)━┭─►(reverse)━┓
 (📈)                   (sort)─────────┶━►(uncurry mappend)

example03 =
  (📈) (+1)━┳━►(+1)━┓
  (📈)      (+1)━━━━╋━►add━┓
  (📈)              add────┶━►add
  where
    add = uncurry (+)

example04 =
  (📈) (+2)━┳━►intToDigit━━━━━━┳━►(>'a')━┓
  (📈)   (show @Int)━►(++"!")─┶━►swap────┶━►id

example05
  :: Diagram '[] '[] (->) Int [((Int, Int), JP.PixelRGB8)]
example05 = undefined
  where
    red   = JP.PixelRGB8 255 0   0
    green = JP.PixelRGB8 0   255 0

example06 =
  (📈)  id┳►                 repeat     ┳► (map (*2))            ━┓
  (📈)  (join (*))━►pred━►(enumFromTo 0)┶►uncurry (zipWith divMod) ┶►uncurry zip

image :: Int -> JP.Image JP.PixelRGB8
image s = runST $ do
    img <- JP.newMutableImage s s
    for (run example05 s) $
        \((x, y), c) -> JP.writePixel img x y c
    JP.freezeImage $ img
