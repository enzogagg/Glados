{-
-- EPITECH PROJECT, 2025
-- Glados
-- File description:
-- TailCallOptimization
-}

module TailCallOptimization (
    Bounce(..),
    runTrampoline
) where

import Types (Value, Env)

data Bounce =
    Done (Either String Value)
    | Call (Env -> IO Bounce) Env

-- =========================
-- TCO
-- =========================

runTrampoline :: Bounce -> IO (Either String Value)
runTrampoline bounce = case bounce of
    Done value -> return value
    Call f env -> do
        newEnv <- f env
        runTrampoline newEnv