module Practice where

import Prelude hiding( product )

product [] = 1
product (x : xs) = x * product xs