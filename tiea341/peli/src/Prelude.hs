module Prelude (module Relude
               ,module Relude.Extra.Foldable1) where
import Relude
import Relude.Extra.Foldable1

{- This is your custom prelude file
   
   * It replaces the old 'Prelude'-library

   * It is a good place to put those functions
     that your project uses in multiple modules,
     but which are not natural in any specific module. 
-}
