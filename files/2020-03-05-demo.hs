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
  (📈)  (id)━┭─►(subtract 0.5)━┳━━━━━►(<0)━━━━━━━━━━━━━━━━┓
  (📈)    (subtract 0.5)───────╆━►uncurry (+)━►abs━►(<0.1)┶►(uncurry (&&))━━━━━━━━┓
  (📈)                      swap━┭─►(*pi)━━►sin ┳()                               ┃
  (📈)                           (*2)───────────┶━►(uncurry (-))━━►abs━━►(<0.2)───┧
  (📈)                                                                   (uncurry (||))━►(bool bg fg)
 where
  fg = JP.PixelRGB8 69  58  98
  bg = JP.PixelRGB8 255 255 255
