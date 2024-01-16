module TurASM.Parser.Getter (parseGetter) where

import TurASM.Parser.Types

import Data.Functor

import Prelude hiding (compare)

import Text.Megaparsec
import Text.Megaparsec.Char

parseGetter :: Parser Getter 
parseGetter = char '&' *> parseGetterName

parseGetterName :: Parser Getter
parseGetterName = selectedSlot 
              <|> itemCount 
              <|> itemSpace 
              <|> detect 
              <|> detectUp 
              <|> detectDown 
              <|> inspect 
              <|> inspectUp 
              <|> inspectDown 
              <|> compare 
              <|> compareUp 
              <|> compareDown 
              <|> fuelLevel 
              <|> fuelLimit

selectedSlot :: Parser Getter
selectedSlot = string "selectedSlot" $> SelectedSlot

itemCount :: Parser Getter 
itemCount = string "itemCount" $> ItemCount 

itemSpace :: Parser Getter 
itemSpace = string "itemSpace" $> ItemSpace 

detect :: Parser Getter 
detect = string "detect" $> Detect 
 
detectUp :: Parser Getter 
detectUp = string "detectUp" $> DetectUp 

detectDown :: Parser Getter 
detectDown = string "detectDown" $> DetectDown 

inspect :: Parser Getter 
inspect = string "inspect" $> Inspect 

inspectUp :: Parser Getter 
inspectUp = string "inspectUp" $> InspectUp 

inspectDown :: Parser Getter 
inspectDown = string "inspectDown" $> InspectDown 

compare :: Parser Getter 
compare = string "compare" $> Compare 

compareUp :: Parser Getter 
compareUp = string "compareUp" $> CompareUp 

compareDown :: Parser Getter 
compareDown = string "compareDown" $> CompareDown 

fuelLevel :: Parser Getter 
fuelLevel = string "fuelLevel" $> FuelLevel 

fuelLimit :: Parser Getter 
fuelLimit = string "fuelLimit" $> FuelLimit
