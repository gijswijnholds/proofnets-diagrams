{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

module Net2Diagram where

import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Maybe
import           Data.Text            (pack)
import           Data.Typeable
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Lucid.Base           (renderBS)
import           ProofNets
import           System.IO

data Dir = L | R deriving (Eq,Show)
data NetNames = N1 | N2 | In | Out | Between | Top | Bottom deriving (Eq,Ord,Show,Typeable)
instance IsName NetNames

writeGraph :: PN i o -> IO ()
writeGraph pn = Strict.writeFile "net.svg" (renderMyGraph' pn)

renderMyGraph :: Strict.ByteString
renderMyGraph = renderMyGraph' testPN1

renderMyGraph' :: PN i o -> Strict.ByteString
renderMyGraph' pn = svg $ proofNets2Diagram pn

proofNets2Diagram :: PN i o -> Diagram SVG
proofNets2Diagram = generalPN2Diagram

svg :: Diagram SVG -> Strict.ByteString
svg = Strict.concat . Lazy.toChunks . renderBS .
      renderDia SVG (SVGOptions (mkWidth 800) Nothing (pack ""))

smallCircle :: Diagram SVG
smallCircle = circle 1 # lwL 0.1

generalPN2Diagram :: PN2Diagram a => PN i o -> Diagram a
generalPN2Diagram (Id rep) = transformId rep
generalPN2Diagram (Comp pn1 pn2) = connectComp (generalPN2Diagram pn1) (generalPN2Diagram pn2)
generalPN2Diagram (MonOtimes pn1 pn2) = connectMonOtimes (generalPN2Diagram pn1) (generalPN2Diagram pn2)
generalPN2Diagram (MonBackslash pn1 pn2) = connectMonBackslash (generalPN2Diagram pn1) (generalPN2Diagram pn2)
generalPN2Diagram (MonSlash pn1 pn2) = connectMonSlash (generalPN2Diagram pn1) (generalPN2Diagram pn2)

class PN2Diagram a where
    transformId :: forall t. Rep t -> Diagram a
    connectComp :: Diagram a -> Diagram a -> Diagram a
    connectMonOtimes :: Diagram a -> Diagram a -> Diagram a
    connectMonBackslash :: Diagram a -> Diagram a -> Diagram a
    connectMonSlash :: Diagram a -> Diagram a -> Diagram a

instance PN2Diagram SVG where
    transformId =  transformId'
    connectComp = connectComp' --x y = x === strutY 3 === y
    connectMonOtimes = connectMon' MOtimes
    connectMonBackslash = connectMon' MBackslash
    connectMonSlash = connectMon' MSlash

transformId' :: forall t. Rep t -> Diagram SVG
transformId' = const $ pointDiagram origin # named In # named Out
connectComp'  :: Diagram SVG -> Diagram SVG -> Diagram SVG
connectComp' d1 d2 = let
                        diagram1 = N1 .>> d1
                        diagram2 = N2 .>> d2
                        modDiagram1 = withName (N1 .> Out) (\sub d -> moveOriginTo (location sub) d) diagram1
                        modDiagram2 = withName (N2 .> In)  (\sub d -> moveOriginTo (location sub) d) diagram2
                        newDiagram' = (modDiagram1 === strutY 3 === modDiagram2)
                        newDiagram = rename (N2 .> Out) Out $ rename (N1 .> In) In newDiagram'
                      in center $ newDiagram
                         # connectPerim' simpleArrowOptions (N1 .> Out) (N2 .> In) (270/360 @@ turn) (90/360 @@ turn)
connectMon' :: MonConnective -> Diagram SVG -> Diagram SVG -> Diagram SVG
connectMon' c d1 d2 = let
  diagram1 = maybeLeftRotate c $ N1 .>> d1
  diagram2 = maybeRightRotate c $ N2 .>> d2
  inNode = smallCircle # named In
  outNode = smallCircle # named Out
  between = strutX 4 # named Between
  maxHeight = 2 + max (height diagram1) (height diagram2)/1.25
  middle = centerY (diagram1 ||| between ||| diagram2)
  matrix = placeBelow maxHeight Between outNode $ placeAbove maxHeight Between inNode middle
  in center $ getArrows matrix c

placeAbove,placeBelow :: IsName nm => Double -> nm -> Diagram SVG -> Diagram SVG -> Diagram SVG
placeAbove i n d1 = withName n $ \sub -> atop $ place d1 $ upper i sub
placeBelow i n d1 = withName n $ \sub -> atop $ place d1 $ lower i sub

upper,lower :: Double -> Subdiagram SVG V2 Double Any -> Point V2 Double
upper i sub = location sub .+^ (i *^ unitY)
lower i = upper (-i)

arrowOptions :: Dir -> ArrowOpts Double
arrowOptions dir = with & arrowShaft .~ getShaft dir & shaftStyle %~ lw thin & arrowHead .~ tri & headLength .~ thin

simpleArrowOptions :: ArrowOpts Double
simpleArrowOptions = with & shaftStyle %~ lw thin & arrowHead .~ tri & headLength .~ thin

rename :: (IsName nm1, IsName nm2) => nm1 -> nm2 -> Diagram SVG -> Diagram SVG
rename n1 = nameSub (fromJust.lookupName n1)

getShaft :: (TrailLike t, V t ~ V2) => Dir -> t
getShaft L = shaftL
getShaft R = shaftR

shaftL,shaftR :: (TrailLike t, V t ~ V2) => t
shaftL = arc xDir (70/360 @@ turn)
shaftR = arc xDir (-70/360 @@ turn)

getArrows,getLeftArrows,getRightArrows :: Diagram SVG -> MonConnective -> Diagram SVG
getArrows d c = getLeftArrows (getRightArrows d c) c
getLeftArrows d c | leftRotate c = d
                    # connectPerim' (arrowOptions R) Out (N1 .> In) (160/360 @@ turn) (270/360 @@ turn)
                    # connectPerim' (arrowOptions R) (N1 .> Out) In (90/360 @@ turn) (200/360 @@ turn)
                  | otherwise    = d
                    # connectPerim' (arrowOptions L) In (N1 .> In) (200/360 @@ turn) (90/360 @@ turn)
                    # connectPerim' (arrowOptions L) (N1 .> Out) Out (270/360 @@ turn) (160/360 @@ turn)
getRightArrows d c | rightRotate c = d
                    # connectPerim' (arrowOptions L) Out (N2 .> In) (20/360 @@ turn) (270/360 @@ turn)
                    # connectPerim' (arrowOptions L) (N2 .> Out) In (90/360 @@ turn) (340/360 @@ turn)
                   | otherwise     = d
                     # connectPerim' (arrowOptions R) In (N2 .> In) (340/360 @@ turn) (90/360 @@ turn)
                     # connectPerim' (arrowOptions R) (N2 .> Out) Out (270/360 @@ turn) (20/360 @@ turn)

maybeLeftRotate, maybeRightRotate :: MonConnective -> Diagram SVG -> Diagram SVG
maybeLeftRotate c d | leftRotate c = rotate (180/360 @@ turn) d
                    | otherwise    = d
maybeRightRotate c d | rightRotate c = rotate (180/360 @@ turn) d
                    | otherwise    = d