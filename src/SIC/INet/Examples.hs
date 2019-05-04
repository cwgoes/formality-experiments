module SIC.INet.Examples where

import           SIC.INet
import           SIC.Type

trivialCommute1 ∷ Net
trivialCommute1 = makeNet
  [ CONS (Ptr 1 P) (Ptr 2 A1) (Ptr 3 A1)
  , DUPL (Ptr 0 P) (Ptr 4 A1) (Ptr 5 A1)
  , FREE (Ptr 0 A1)
  , FREE (Ptr 0 A2)
  , FREE (Ptr 1 A1)
  , FREE (Ptr 1 A2)
  ]

trivialCommute2 ∷ Net
trivialCommute2 = makeNet
  [ CONS (Ptr 0 P) (Ptr 1 A1) (Ptr 2 A1)
  , FREE (Ptr 0 A1)
  , FREE (Ptr 0 A2)
  ]

trivialCommute3 ∷ Net
trivialCommute3 = makeNet
  [ DUPL (Ptr 0 P) (Ptr 1 A1) (Ptr 2 A1)
  , FREE (Ptr 0 A1)
  , FREE (Ptr 0 A2)
  ]

trivialAnnihilate1 ∷ Net
trivialAnnihilate1 = makeNet
  [ CONS (Ptr 1 P) (Ptr 2 A1) (Ptr 3 A1)
  , CONS (Ptr 0 P) (Ptr 4 A1) (Ptr 5 A1)
  , FREE (Ptr 0 A1)
  , FREE (Ptr 0 A2)
  , FREE (Ptr 1 A1)
  , FREE (Ptr 1 A2)
  ]


trivialAnnihilate2 ∷ Net
trivialAnnihilate2 = makeNet
  [ CONS (Ptr 1 P) (Ptr 2 A1) (Ptr 3 A1)
  , CONS (Ptr 0 P) (Ptr 4 A1) (Ptr 5 A1)
  , FREE (Ptr 0 A1)
  , FREE (Ptr 0 A2)
  , FREE (Ptr 1 A1)
  , FREE (Ptr 1 A2)
  ]

--trivialAnnihilate3 ∷ Net
--trivialAnnihilate3 = makeNet [
--  (Eraser, [Pointer 1 0]),
--  (Eraser, [Pointer 0 0])
--  ]
