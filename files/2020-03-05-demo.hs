{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Demo where

import Visual

import Data.List
import Data.Char
import Data.Bool (bool)
import Control.Monad (join)
import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Bifunctor (bimap, second, first)
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
$(industryStandardBoilerPlate (Just '━') '(╋►) (Just '━')) -- Needs small up parts?
$(industryStandardBoilerPlate (Just '━') '(┳►) (Just '━'))
$(industryStandardBoilerPlate (Just '─') '(┧) Nothing)

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
red   = JP.PixelRGB8 255 0   0
green = JP.PixelRGB8 0   255 0

{-
both f (a, b) = (f a, f b)

example06 =
  (📈)  id┳►                 repeat     ┳► id                     ━┓
  (📈)  (join (*))━►pred━►(enumFromTo 0)┶►uncurry (zipWith divMod) ┧
-}

example07 =
  (📈)  id┭►(subtract 0.5)┳►(*pi)━►sin━┓
  (📈)  (*2)━►(pred)      ╋►(fst)      ┶►(uncurry (-))━►abs━►(<0.1)━┓
  (📈)                (second(*2))━►uncurry (+)━►abs━►(<0.1)        ┧
  (📈)                                          (uncurry (||)) ━► (bool red green)

example08 =
  (📈)  (id)━┭─►(subtract 0.5)━┳━━━━━►(<0)━━━━━━━━━━━━━━━━┓
  (📈)    (subtract 0.5)━━━━━━━╋━►uncurry (+)━►abs━►(<0.1)┶►(uncurry (&&))━┓
  (📈)                      swap━┭─►(*pi)━━►sin━┳━━━━━━━━►const ()─────────┶━►snd━┓
  (📈)                           (*2)───────────┶━►(uncurry (-))━━►abs━━►(<0.2)───┧
  (📈)                                                                   (uncurry (||))━►(bool red green)

example09 =
  (📈)  (id)━┭─►(subtract 0.5)━┳━━━━━►(<0)━━━━━━━━━━━━━━━━┓
  (📈)    (subtract 0.5)━━━━━━━╋━►uncurry (+)━►abs━►(<0.1)┶►(uncurry (&&))━━━━━━━━┓
  (📈)                      swap━┭─►(*pi)━━►sin ┳()                               ║
  (📈)                           (*2)───────────┶━►(uncurry (-))━━►abs━━►(<0.2)───┧
  (📈)                                                                   (uncurry (||))━►(bool red green)

{-
  ┭►(subtract 0.5)┳►(*pi)━►sin━┓
  (📈)  (*2)━►(pred)      ╋►(fst)      ┶►(uncurry (-))━►abs━►(<0.1)━┓
  (📈)                (second(*2))━►uncurry (+)━►abs━►(<0.1)        ┧
  (📈)                                          (uncurry (||)) ━► (bool red green)
  -}

image :: Int -> JP.Image JP.PixelRGB8
image s = runST $ do
    img <- JP.newMutableImage s s
    let sd = fromIntegral (s - 1) :: Double
    for_ [0 .. s - 1] $ \y ->
        for_ [0 .. s - 1] $ \x ->
            JP.writePixel img x y $ run example08
                (fromIntegral x / sd, fromIntegral y / sd)
    JP.freezeImage $ img
