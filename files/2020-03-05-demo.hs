{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Demo where

import Visual

import Data.Bool (bool)
import Data.Tuple
import qualified Codec.Picture.Types as JP

$(industryStandardBoilerplate (Just '━') '(━►) Nothing)
$(industryStandardBoilerplate (Just '━') '(┭►) (Just '─'))
$(industryStandardBoilerplate (Just '━') '(┓) Nothing)
$(industryStandardBoilerplate (Just '─') '(┶►) (Just '━'))
$(industryStandardBoilerplate (Just '─') '(╆►) (Just '━')) -- Needs small up parts?
$(industryStandardBoilerplate (Just '━') '(┳►) (Just '━'))
$(industryStandardBoilerplate (Just '─') '(┧) Nothing)

lambda =
  (📈)  (id)━┭─►(subtract 0.5)━┳━━━━━━━━►(< 0)━━━━━━━━━━┓
  (📈)    (subtract 0.5)───────╆━►(add)━►(abs)━►(< 0.1)─┶━━━━━►(and)━━━━━━┓
  (📈)                      (swap)━┭─►(* pi)━━►(sin)┳()                   ┃
  (📈)                           ( *2)──────────────┶━►(sub)━►abs━►(<0.2)─┧
  (📈)                                                                  (or)━►(bool bg fg)
 where
  add = uncurry (+)
  sub = uncurry (-)
  and = uncurry (&&)
  or  = uncurry (||)
  fg  = JP.PixelRGB8 69  58  98
  bg  = JP.PixelRGB8 255 255 255
