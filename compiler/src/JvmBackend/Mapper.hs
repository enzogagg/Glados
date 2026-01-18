module JvmBackend.Mapper (mapOp) where

mapOp :: String -> String -> String
mapOp "entier" "+" = "iadd"
mapOp "entier" "-" = "isub"
mapOp "entier" "*" = "imul"
mapOp "entier" "/" = "idiv"
mapOp "flottant" "+" = "fadd"
mapOp "flottant" "-" = "fsub"
mapOp "flottant" "*" = "fmul"
mapOp "flottant" "/" = "fdiv"
mapOp _ _ = "nop"