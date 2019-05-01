module INet.Examples where

import           INet.SIC

trivialCommute1 ∷ Net
trivialCommute1 = makeNet [
  (Constructor, [Pointer 1 0, Free "a", Free "b"]),
  (Duplicator, [Pointer 0 0, Free "c", Free "d"])
  ]

trivialCommute2 ∷ Net
trivialCommute2 = makeNet [
  (Constructor, [Pointer 1 0, Free "a", Free "b"]),
  (Eraser, [Pointer 0 0])
  ]

trivialCommute3 ∷ Net
trivialCommute3 = makeNet [
  (Duplicator, [Pointer 1 0, Free "a", Free "b"]),
  (Eraser, [Pointer 0 0])
  ]

trivialAnnihilate1 ∷ Net
trivialAnnihilate1 = makeNet [
  (Constructor, [Pointer 1 0, Free "a", Free "b"]),
  (Constructor, [Pointer 0 0, Free "c", Free "d"])
  ]

trivialAnnihilate2 ∷ Net
trivialAnnihilate2 = makeNet [
  (Duplicator, [Pointer 1 0, Free "a", Free "b"]),
  (Duplicator, [Pointer 0 0, Free "c", Free "d"])
  ]

trivialAnnihilate3 ∷ Net
trivialAnnihilate3 = makeNet [
  (Eraser, [Pointer 1 0]),
  (Eraser, [Pointer 0 0])
  ]
