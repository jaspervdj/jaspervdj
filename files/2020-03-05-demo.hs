{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Demo where

import Visual

import Data.List
import Data.Char
import Data.Tuple
import qualified Codec.Picture.Png as Png

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
  (📈)    (show @Int)━►(++"!")─┶━►swap───┶━►id
