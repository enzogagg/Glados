{-
-- EPITECH PROJECT, 2025
-- Delivery
-- File description:
-- Complex data structures operations
-}

module Execution.Ops.ComplexDataStructures where

import Types (Value(..))
import Execution.State (VMState(..))

opMakeTuple :: Int -> VMState -> Either String VMState
opMakeTuple n state
    | n < 0 = Left "Error: Tuple size cannot be negative"
    | length (stack state) < n = Left "Error: Not enough values on the stack"
    | otherwise = Right $ state { stack = TupleVal (reverse (take n (stack state))) : drop n (stack state) }

opTupleGet :: Int -> VMState -> Either String VMState
opTupleGet idx state = case stack state of
    (TupleVal values : rest) -> if idx >= 0 && idx < length values
        then Right $ state { stack = (values !! idx) : rest }
        else Left ("Error: Index " ++ show idx ++ " out of bounds for Tuple size " ++ show (length values))
    (_ : _) -> Left "Error: TupleGet expects a Tuple at the top of the stack"
    [] -> Left "Error: Stack empty"

opMakeArray :: Int -> VMState -> Either String VMState
opMakeArray n state
    | n < 0 = Left "Error: Array size cannot be negative"
    | length (stack state) < n = Left "Error: Not enough values on the stack"
    | otherwise = Right $ state { stack = ArrayVal (reverse (take n (stack state))) : drop n (stack state) }

opArrayGet :: VMState -> Either String VMState
opArrayGet state = case stack state of
    (IntVal idx : ArrayVal values : rest) ->
        if idx >= 0 && idx < length values
        then Right $ state { stack = (values !! idx) : rest }
        else Left ("Error: Index " ++ show idx ++ " out of bounds for Array")
    (_ : _ : _) -> Left "Error: ArrayGet expects an Integer (index) and an Array on the stack"
    _ -> Left "Error: Stack underflow for ArrayGet"

opArraySet :: VMState -> Either String VMState
opArraySet state = case stack state of
    (val : IntVal idx : ArrayVal values : rest) ->
        if idx >= 0 && idx < length values
        then 
            let (before, (_:after)) = splitAt idx values
                newArray = ArrayVal (before ++ [val] ++ after)
            in Right $ state { stack = newArray : rest }
        else Left ("Error: Index " ++ show idx ++ " out of bounds for Array")
    (_ : _ : _ : _) -> Left "Error: ArraySet expects Value, Integer (index), and Array on stack"
    _ -> Left "Error: Stack underflow for ArraySet"

opMakeMap :: Int -> VMState -> Either String VMState
opMakeMap n state
    | n < 0 = Left "Error: Map size cannot be negative"
    | length (stack state) < n * 2 = Left "Error: Not enough values on the stack (need (k,v) pairs)"
    | otherwise = 
        let rawValues = take (n * 2) (stack state)
            pairs = toPairs rawValues
        in Right $ state { stack = MapVal pairs : drop (n * 2) (stack state) }
    where
        toPairs [] = []
        toPairs (k:v:rest) = (k,v) : toPairs rest
        toPairs _ = []

opMapGet :: VMState -> Either String VMState
opMapGet state = case stack state of
    (key : MapVal pairs : rest) ->
        case lookup key pairs of
            Just val -> Right $ state { stack = val : rest }
            Nothing -> Left ("Error: Key not found in Map: " ++ show key)
    (_ : _ : _) -> Left "Error: MapGet expects Key and Map on stack"
    _ -> Left "Error: Stack underflow for MapGet"

opMapSet :: VMState -> Either String VMState
opMapSet state = case stack state of
    (val : key : MapVal pairs : rest) ->
        let newPairs = (key, val) : filter (\(k, _) -> k /= key) pairs
        in Right $ state { stack = MapVal newPairs : rest }
    (_ : _ : _ : _) -> Left "Error: MapSet expects Value, Key, and Map on stack"
    _ -> Left "Error: Stack underflow for MapSet"

opMakeStruct :: Int -> VMState -> Either String VMState
opMakeStruct n state
    | n < 0 = Left "Error: Struct size cannot be negative"
    | length (stack state) < n = Left "Error: Not enough values on the stack"
    | otherwise = 
        let required = n * 2 
        in if length (stack state) < required 
           then Left "Error: Not enough values for Struct (need key(string)-value pairs)" 
           else
            let rawValues = take required (stack state)
                pairs = toStructPairs rawValues
            in case pairs of
                Right p -> Right $ state { stack = StructVal p : drop required (stack state) }
                Left err -> Left err
    where
        toStructPairs [] = Right []
        toStructPairs (StringVal k : v : rest) = 
            case toStructPairs rest of
                Right ps -> Right ((k, v) : ps)
                Left e -> Left e
        toStructPairs (_ : _ : _) = Left "Error: Struct keys must be Strings"
        toStructPairs _ = Right []

opStructGet :: String -> VMState -> Either String VMState
opStructGet key state = case stack state of
    (StructVal fields : rest) ->
        case lookup key fields of
            Just val -> Right $ state { stack = val : rest }
            Nothing -> Left ("Error: Field not found in Struct: " ++ key)
    (_ : _) -> Left "Error: StructGet expects a Struct at the top of the stack"
    [] -> Left "Error: Stack empty"

opStructSet :: String -> VMState -> Either String VMState
opStructSet key state = case stack state of
    (val : StructVal fields : rest) ->
        let newFields = (key, val) : filter (\(k, _) -> k /= key) fields
        in Right $ state { stack = StructVal newFields : rest }
    (_ : _) -> Left "Error: StructSet expects Value and Struct at the top of the stack"
    [] -> Left "Error: Stack empty"

