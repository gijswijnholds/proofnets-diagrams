{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
module ProofNets where

-- small test stuff on top
testPN1 = Comp (Id (RAt A)) (Id (RAt A))

testPN2 :: PN (Rep (FROtimes A B)) (Rep (FROtimes A B))
testPN2 = MonOtimes (Id (RAt A)) (Id (RAt B))

testPN3 = Comp testPN2 testPN2
testPN4 = MonOtimes testPN3 testPN1

testPN5 = MonSlash testPN1 testPN3
-- end small test stuff

class Show a => Atom a where

data A = A deriving Show
data B = B deriving Show
data C = C deriving Show
data D = D deriving Show

instance Atom A where
instance Atom B where
instance Atom C where
instance Atom D where

data FROtimes a b = FROtimes a b
data FRBackslash a b = FRBackslash a b
data FRSlash a b = FRSlash a b

data Rep t where
    RAt :: (Atom a) => a -> Rep a
    ROtimes :: Rep a -> Rep b -> Rep (FROtimes a b)
    RBackslash :: Rep a -> Rep b -> Rep (FRBackslash a b)
    RSlash :: Rep a -> Rep b -> Rep (FRSlash a b)

data MonConnective =
                 MOtimes
                 | MBackslash
                 | MSlash
                 deriving (Eq, Show)

class Rotation a where
  leftRotate :: a -> Bool
  rightRotate :: a -> Bool

instance Rotation MonConnective where
  leftRotate MBackslash = True
  leftRotate _ = False
  rightRotate MSlash = True
  rightRotate _ = False

-- Get regular formula from representation

data Formula where
    At :: (Atom a) => a -> Formula
    Otimes :: Formula -> Formula -> Formula
    Backslash :: Formula -> Formula -> Formula
    Slash :: Formula -> Formula -> Formula

instance Show Formula where
    show (At a) = show a
    show (Otimes f1 f2) = show f1 ++ " (*) " ++ show f2

fromFRep :: Rep t -> Formula
fromFRep (RAt s) = At s
fromFRep (ROtimes rA rB) = Otimes (fromFRep rA) (fromFRep rB)
fromFRep (RBackslash rA rB) = Backslash (fromFRep rA) (fromFRep rB)
fromFRep (RSlash rA rB) = Slash (fromFRep rA) (fromFRep rB)

-- Define proofnets

data PN i o where
    Id              :: Rep t -> PN (Rep t) (Rep t)
    Comp            :: PN (Rep a) (Rep b) -> PN (Rep b) (Rep c) -> PN (Rep a) (Rep c)
    MonOtimes       :: PN (Rep a) (Rep c) -> PN (Rep b) (Rep d) -> PN (Rep (FROtimes a b)) (Rep (FROtimes c d))
    MonBackslash    :: PN (Rep a) (Rep c) -> PN (Rep b) (Rep d) -> PN (Rep (FRBackslash c b)) (Rep (FRBackslash a d))
    MonSlash        :: PN (Rep a) (Rep c) -> PN (Rep b) (Rep d) -> PN (Rep (FRSlash a d)) (Rep (FRSlash c b))
    LeftApp         :: PN (Rep a) (Rep c) -> PN (Rep b) (Rep (FRBackslash c d)) -> PN (Rep (FROtimes a b)) (Rep d)
    RightApp        :: PN (Rep a) (Rep (FRSlash d c)) -> PN (Rep b) (Rep c) -> PN (Rep (FROtimes a b)) (Rep d)

-- SMART CONSTRUCTORS HERE

identity = Id
compose = Comp
monOtimes = MonOtimes
monBackslash = MonBackslash
monSlash = MonSlash
leftApp = LeftApp
rightApp = RightApp

inputRep :: PN (Rep i) (Rep o) -> Rep i
inputRep (Id rep) = rep
inputRep (Comp pn1 _) = inputRep pn1
inputRep (MonOtimes pn1 pn2) = ROtimes (inputRep pn1) (inputRep pn2)

outputRep :: PN (Rep i) (Rep o) -> Rep o
outputRep (Id rep) = rep
outputRep (Comp _ pn2) = outputRep pn2
outputRep (MonOtimes pn1 pn2) = ROtimes (outputRep pn1) (outputRep pn2)

-- Simple Proof Net destructions
inputFormula :: PN (Rep i) (Rep o) -> Formula
inputFormula = fromFRep.inputRep

outputFormula :: PN (Rep i) (Rep o) -> Formula
outputFormula = fromFRep.outputRep
