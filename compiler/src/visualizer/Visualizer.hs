module Visualizer (astToDot) where

import Types
import Data.List (intercalate)

astToDot :: AST -> String
astToDot tree =
    "digraph AST {\n" ++
    "  node [fontname=\"Arial\", shape=box, style=filled, fillcolor=white];\n" ++
    "  edge [fontname=\"Arial\", fontsize=10];\n" ++
    snd (go tree 0) ++
    "}"
  where
    go :: AST -> Int -> (Int, String)

    go (IANumber n) i =
        (i + 1, "  node" ++ show i ++ " [label=\"Nombre: " ++ show n ++ "\", fillcolor=\"#e1f5fe\"];\n")

    go (IAFloatLiteral n) i =
        (i + 1, "  node" ++ show i ++ " [label=\"Flottant: " ++ show n ++ "\", fillcolor=\"#e1f5fe\"];\n")

    go (IAString s) i =
        (i + 1, "  node" ++ show i ++ " [label=\"Phrase: \\\"" ++ s ++ "\\\"\", fillcolor=\"#fff9c4\"];\n")

    go (IABoolean b) i =
        (i + 1, "  node" ++ show i ++ " [label=\"Bool: " ++ show b ++ "\", fillcolor=\"#f3e5f5\"];\n")

    go (IASymbol s) i =
        (i + 1, "  node" ++ show i ++ " [label=\"Symbole: " ++ s ++ "\", color=blue, fontcolor=blue];\n")

    go (IAInfix left op right) i =
        let (nextI1, leftStr) = go left (i + 1)
            (nextI2, rightStr) = go right nextI1
            currentStr = "  node" ++ show i ++ " [label=\"" ++ op ++ "\", shape=diamond, fillcolor=\"#eeeeee\"];\n" ++
                         "  node" ++ show i ++ " -> node" ++ show (i + 1) ++ " [label=\"L\"];\n" ++
                         "  node" ++ show i ++ " -> node" ++ show nextI1 ++ " [label=\"R\"];\n"
        in (nextI2, currentStr ++ leftStr ++ rightStr)

    go (IAIf cond thenB elseB) i =
        let (nextI1, condStr) = go cond (i + 1)
            (nextI2, thenStr) = go (IAProgram thenB) nextI1
            resStr = "  node" ++ show i ++ " [label=\"SI\", fillcolor=\"#fff176\"];\n" ++
                     "  node" ++ show i ++ " -> node" ++ show (i + 1) ++ " [label=\"cond\"];\n" ++
                     "  node" ++ show i ++ " -> node" ++ show nextI1 ++ " [label=\"alors\"];\n"
        in case elseB of
            Nothing -> (nextI2, resStr ++ condStr ++ thenStr)
            Just b ->
                let (nextI3, elseStr) = go (IAProgram b) nextI2
                    finalStr = resStr ++ "  node" ++ show i ++ " -> node" ++ show nextI2 ++ " [label=\"sinon\"];\n"
                in (nextI3, finalStr ++ condStr ++ thenStr ++ elseStr)

    go (IAWhile cond body) i =
        let (nextI1, condStr) = go cond (i + 1)
            (nextI2, bodyStr) = go (IAProgram body) nextI1
            currentStr = "  node" ++ show i ++ " [label=\"TANTQUE\", fillcolor=\"#fff176\"];\n" ++
                         "  node" ++ show i ++ " -> node" ++ show (i + 1) ++ " [label=\"cond\"];\n" ++
                         "  node" ++ show i ++ " -> node" ++ show nextI1 ++ " [label=\"corps\"];\n"
        in (nextI2, currentStr ++ condStr ++ bodyStr)

    go (IADeclare name _ expr) i =
        let (nextI, exprStr) = go expr (i + 1)
            currentStr = "  node" ++ show i ++ " [label=\"Déclarer: " ++ name ++ "\", fillcolor=\"#c8e6c9\"];\n" ++
                         "  node" ++ show i ++ " -> node" ++ show (i + 1) ++ ";\n"
        in (nextI, currentStr ++ exprStr)

    go (IAAssign name expr) i =
        let (nextI, exprStr) = go expr (i + 1)
            currentStr = "  node" ++ show i ++ " [label=\"Assigner: " ++ name ++ "\", fillcolor=\"#c8e6c9\"];\n" ++
                         "  node" ++ show i ++ " -> node" ++ show (i + 1) ++ ";\n"
        in (nextI, currentStr ++ exprStr)

    go (IAFunctionDef name params _ body) i =
        let paramNames = intercalate ", " [pName | (pName, _) <- params]
            label = "Fonction: " ++ name ++ "\\nArgs: (" ++ paramNames ++ ")"
            (nextI, bodyStr) = go (IAProgram body) (i + 1)
            currentStr = "  node" ++ show i ++ " [label=\"" ++ label ++ "\", fillcolor=\"#ffccbc\"];\n" ++
                         "  node" ++ show i ++ " -> node" ++ show (i + 1) ++ ";\n"
        in (nextI, currentStr ++ bodyStr)

    go (IAMain args body) i =
        let label = "Principal(" ++ intercalate ", " args ++ ")"
            (nextI, bodyStr) = go (IAProgram body) (i + 1)
            currentStr = "  node" ++ show i ++ " [label=\"" ++ label ++ "\", fillcolor=\"#d1c4e9\"];\n" ++
                         "  node" ++ show i ++ " -> node" ++ show (i + 1) ++ ";\n"
        in (nextI, currentStr ++ bodyStr)

    go (IACall name args) i =
        let folder (currentId, accStr, childIds) arg =
                let (newId, newStr) = go arg currentId
                in (newId, accStr ++ newStr, childIds ++ [currentId])
            (lastId, allArgsStr, ids) = foldl folder (i + 1, "", []) args
            connect = concatMap (\childId -> "  node" ++ show i ++ " -> node" ++ show childId ++ ";\n") ids
            currentStr = "  node" ++ show i ++ " [label=\"Appel: " ++ name ++ "\", color=green];\n" ++ connect
        in (lastId, currentStr ++ allArgsStr)

    go (IAList exprs) i =
        let folder (currentId, accStr, childIds) expr =
                let (newId, newStr) = go expr currentId
                in (newId, accStr ++ newStr, childIds ++ [currentId])
            (lastId, allStr, ids) = foldl folder (i + 1, "", []) exprs
            connect = concatMap (\childId -> "  node" ++ show i ++ " -> node" ++ show childId ++ ";\n") ids
            currentStr = "  node" ++ show i ++ " [label=\"Liste\", shape=parallelogram];\n" ++ connect
        in (lastId, currentStr ++ allStr)

    go (IAProgram instrs) i =
        let folder (currentId, accStr, childIds) inst =
                let (newId, newStr) = go inst currentId
                in (newId, accStr ++ newStr, childIds ++ [currentId])
            (lastId, allNodesStr, ids) = foldl folder (i + 1, "", []) instrs
            connect = concatMap (\childId -> "  node" ++ show i ++ " -> node" ++ show childId ++ ";\n") ids
            currentStr = "  node" ++ show i ++ " [label=\"Bloc\", shape=box3d];\n" ++ connect
        in (lastId, currentStr ++ allNodesStr)

    go (IAReturn expr) i =
        let (nextI, exprStr) = go expr (i + 1)
            currentStr = "  node" ++ show i ++ " [label=\"Retourner\", fillcolor=\"#ffcdd2\"];\n" ++
                         "  node" ++ show i ++ " -> node" ++ show (i + 1) ++ ";\n"
        in (nextI, currentStr ++ exprStr)

    go _ i = (i + 1, "  node" ++ show i ++ " [label=\"Autre nœud\"];\n")