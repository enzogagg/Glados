module JvmBackend.Mapper (mapOp) where

mapOp :: String -> String
mapOp "+"  = "iadd"
mapOp "-"  = "isub"
mapOp "*"  = "imul"
mapOp "/"  = "idiv"
mapOp "==" = "if_icmpeq"
mapOp "!=" = "if_icmpne"
mapOp "<"  = "if_icmplt"
mapOp ">"  = "if_icmpgt"
mapOp _    = "nop"