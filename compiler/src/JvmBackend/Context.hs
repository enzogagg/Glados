module JvmBackend.Context where

import Control.Monad.State
import qualified Data.Map.Strict as Map

data JvmContext = JvmContext
    { varMap :: Map.Map String Int
    , nextVarIndex :: Int
    , labelCount :: Int
    } deriving (Show)

type JvmGen = State JvmContext String

emptyContext :: JvmContext
emptyContext = JvmContext Map.empty 0 0

reserveVar :: String -> State JvmContext Int
reserveVar name = do
    ctx <- get
    let idx = nextVarIndex ctx
    put ctx { varMap = Map.insert name idx (varMap ctx)
            , nextVarIndex = idx + 1 }
    return idx

uniqueLabel :: String -> State JvmContext String
uniqueLabel prefix = do
    ctx <- get
    let count = labelCount ctx
    put ctx { labelCount = count + 1 }
    return $ prefix ++ "_" ++ show count

getVarIndex :: String -> State JvmContext (Maybe Int)
getVarIndex name = gets (Map.lookup name . varMap)