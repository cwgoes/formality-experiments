module SIC.INet.Examples where

import           SIC.INet
import           SIC.Type

trivialCommute1 ∷ Net
trivialCommute1 = makeNet
  [ CONS (Ptr 1 P) (Ptr 2 L) (Ptr 3 L)
  , DUPL (Ptr 0 P) (Ptr 4 L) (Ptr 5 L)
  , FREE (Ptr 0 L)
  , FREE (Ptr 0 R)
  , FREE (Ptr 1 L)
  , FREE (Ptr 1 R)
  ]

trivialCommute2 ∷ Net
trivialCommute2 = makeNet
  [ CONS (Ptr 0 P) (Ptr 1 L) (Ptr 2 L)
  , FREE (Ptr 0 L)
  , FREE (Ptr 0 R)
  ]

trivialCommute3 ∷ Net
trivialCommute3 = makeNet
  [ DUPL (Ptr 0 P) (Ptr 1 L) (Ptr 2 L)
  , FREE (Ptr 0 L)
  , FREE (Ptr 0 R)
  ]

trivialAnnihilate1 ∷ Net
trivialAnnihilate1 = makeNet
  [ CONS (Ptr 1 P) (Ptr 2 L) (Ptr 3 L)
  , CONS (Ptr 0 P) (Ptr 4 L) (Ptr 5 L)
  , FREE (Ptr 0 L)
  , FREE (Ptr 0 R)
  , FREE (Ptr 1 L)
  , FREE (Ptr 1 R)
  ]


trivialAnnihilate2 ∷ Net
trivialAnnihilate2 = makeNet
  [ CONS (Ptr 1 P) (Ptr 2 L) (Ptr 3 L)
  , CONS (Ptr 0 P) (Ptr 4 L) (Ptr 5 L)
  , FREE (Ptr 0 L)
  , FREE (Ptr 0 R)
  , FREE (Ptr 1 L)
  , FREE (Ptr 1 R)
  ]

--trivialAnnihilate3 ∷ Net
--trivialAnnihilate3 = makeNet [
--  (Eraser, [Pointer 1 0]),
--  (Eraser, [Pointer 0 0])
--  ]
