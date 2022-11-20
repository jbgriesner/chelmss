module Utils exposing (..)
import Bitwise exposing (or)

-- ME

type M a = T (a,a,a) | L (List a) | No

unlist : M a -> List a
unlist x = case x of
    L l -> l
    _ -> []

bind : M c -> (c -> M b) -> M b
bind u v = case u of
    No -> No
    T (h, i, j) -> case (v h, v i, v j) of
                    (T ((h1, h2, h3)), T ((i1, i2, i3)), T ((j1, j2, j3))) -> T (h1, i2, j3)
                    _ -> No
    L las -> let r = List.map v las 
                        |> List.map unlist |> List.concat 
             in L r
z = T (1, 2, 3)
y = L [1, 2, 3, 4]
q = No

uu : Int -> M Int
uu x = L [x]

-- Type Classes

type alias Functor x a b = {fmap : (a -> b) -> x -> x}

type Tree a = Node (Tree a) (Tree a) | Leaf

treeMap : Functor (Tree Int) Int Int
treeMap = {
    fmap = \f t -> case t of
        Node (Tree t1) (Tree t2) -> Node (Tree f t1) (Tree f t2)
        Leaf -> Leaf
}

showTree : Tree a -> String
showTree t = case t of
    Node x y -> String.split ("Node\n  --> " ++ (showTree x) ++ "\n  --> " ++ (showTree x))
    Leaf -> "Leaf"   