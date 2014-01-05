import Data.Sequence as S
-- import Data.Tree

data BTree a  = Node (Seq a) | Branch (Buffer a) (BTree a) (BTree a)
type Buffer = Seq

kfunnel :: Seq (Seq a) -> Seq a
kfunnel ss = undefined
           where k = fromIntegral $ S.length ss
                 subk = ceiling (k ** 1.5) :: Int

{-

we take a list of length N

we chop it up into n^1/3 segments,
that is, [10,9,8,7,6,5,4,3,2,1] with length 10
10**(1/3) ~> 3
so 3 chunks
each chunk has length:
10**(2/3) ~> 4
so
 chunkify [10,9,8,7,6,5,4,3,2,1] ~>
[
 [10,9,8,7],
 [6, 5,4,3],
 [2,1]
]

then we sort each sublist:

[
 [7,8,9,10],
 [3,4,5, 6],
 [1,2]
]

now, we make the list into a  binary tree:

                  root[]
            /              \
           node[]          node[]
          /      \          /  \
    [7,8,9,10]   [3,4,5,6] []  [1,2]

we go down left first, and we merge the two easily:


               root[]
            /            \
 [3,4,5,6,7,8,9,10]      node[]
                         /    \
                        []    [1,2]

then we go down right:

               root[]
            /          \
 [3,4,5,6,7,8,9,10]    [1,2]

then we do a final merge:


      root [1,2,3,4,5,6,7,8,9,10]

and that's our answer


[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
n = 16
k = 3

16^(1/k)
16^1/3
5.33...
ceiling(16^1/3)
6

so we have 6 leaves

    root     (1)
    / \
   a   b     (2)
  /\  /\
 c   d  e    (3)
/\  /\  /\
1 2 3 4 5 6  (4)

(12 nodes in total)

1-6 must each handle n/6 items
or ceiling(16/6) items or 3 items a piece

so, we construct the binary tree for leaf #1:

3**1/3 = 1

oh, that's good. we bottom out here. so each of our leaves is sorted (if it
isn't already):

1: [1,2,3]
2: [4,5,6]
3: [7,8,9]
4: [10,11,12]
5: [13,14,15]
6: [16]

we call fill on root

-}
