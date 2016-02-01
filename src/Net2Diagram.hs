{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Net2Diagram where

import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.Tuple
import           Data.Maybe
import           Data.Text            (pack)
import           Data.Typeable
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Lucid.Base           (renderBS)
import           ProofNets
import           System.IO

data Dir = L | R deriving (Eq,Show)
data NetNames = N1 | N2 | In | Out | Between | Top | Bottom | Point Int deriving (Eq,Ord,Show,Typeable)
instance IsName NetNames

writeDiagram :: Diagram SVG -> IO ()
writeDiagram d = Strict.writeFile "net1.svg" $ svg d

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

connCircle :: MonConnective -> Diagram SVG
connCircle c = showDiagram c # scale 0.5 `atop` smallCircle

showDiagram :: MonConnective -> Diagram SVG
showDiagram MOtimes = (vrule 2 # rotate (1/8 @@ turn)) <> (hrule 2 # rotate (1/8 @@ turn)) <> circle 1
showDiagram MSlash = vrule 2 # rotate (-1/16 @@ turn)
showDiagram MBackslash = vrule 2 #rotate (1/16 @@ turn)


generalPN2Diagram :: PN2Diagram a => PN i o -> Diagram a
generalPN2Diagram (Id rep) = transformId rep
generalPN2Diagram (Comp pn1 pn2) = connectComp (generalPN2Diagram pn1) (generalPN2Diagram pn2)
generalPN2Diagram (MonOtimes pn1 pn2) = connectMonOtimes (generalPN2Diagram pn1) (generalPN2Diagram pn2)
generalPN2Diagram (MonBackslash pn1 pn2) = connectMonBackslash (generalPN2Diagram pn1) (generalPN2Diagram pn2)
generalPN2Diagram (MonSlash pn1 pn2) = connectMonSlash (generalPN2Diagram pn1) (generalPN2Diagram pn2)
generalPN2Diagram (LeftApp pn1 pn2) = connectLeftApp (generalPN2Diagram pn1) (generalPN2Diagram pn2)
generalPN2Diagram (RightApp pn1 pn2) = connectRightApp (generalPN2Diagram pn1) (generalPN2Diagram pn2)

class PN2Diagram a where
    transformId :: forall t. Rep t -> Diagram a
    connectComp :: Diagram a -> Diagram a -> Diagram a
    connectMonOtimes :: Diagram a -> Diagram a -> Diagram a
    connectMonBackslash :: Diagram a -> Diagram a -> Diagram a
    connectMonSlash :: Diagram a -> Diagram a -> Diagram a
    connectLeftApp :: Diagram a -> Diagram a -> Diagram a
    connectRightApp :: Diagram a -> Diagram a -> Diagram a

instance PN2Diagram SVG where
    transformId =  transformId'
    connectComp = connectComp'
    connectMonOtimes = connectMon' MOtimes
    connectMonBackslash = connectMon' MBackslash
    connectMonSlash = connectMon' MSlash
    connectLeftApp = connectLeftApp'
    connectRightApp = connectRightApp'

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
  inNode = connCircle c # maybeColorGrayTop c # named In
  outNode = connCircle c # maybeColorGrayBottom c # named Out
  between = strutX 4 # named Between
  maxHeight = calcMaxHeight diagram1 diagram2
  middle = centerY (diagram1 ||| between ||| diagram2)
  matrix = placeBelow maxHeight Between outNode $ placeAbove maxHeight Between inNode middle
  in center $ getArrows matrix c

connectLeftApp' :: Diagram SVG -> Diagram SVG -> Diagram SVG
connectLeftApp' d1 d2 = let
  diagram1 = N1 .>> d1
  diagram2 = N2 .>> d2
  inNode = connCircle MOtimes # named In
  point1 = pointDiagram' # named (Point 1)
  point2 = pointDiagram' # named (Point 2)
  point3 = connCircle MBackslash # named (Point 3)
  outNode = pointDiagram' # named Out
  between = strutX 4 # named Between
  underConstant = 4
  maxHeight = calcMaxHeight diagram1 diagram2
  middle = centerY (diagram1 ||| between ||| diagram2)
  matrix = placeBelowRight (2*underConstant + maxHeight) underConstant Between outNode
         $ placeBelow (underConstant + maxHeight) Between point2
         $ placeBelow underConstant (N2 .> Out) point3
         $ placeBelow underConstant (N1 .> Out) point1
         $ placeAbove maxHeight Between inNode middle
  in center $ buildArrowsLeftApp matrix

connectRightApp' :: Diagram SVG -> Diagram SVG -> Diagram SVG
connectRightApp' d1 d2 = let
    diagram1 = N1 .>> d1
    diagram2 = N2 .>> d2
    inNode = connCircle MOtimes # named In
    point1 = connCircle MSlash # named (Point 1)
    point2 = pointDiagram' # named (Point 2)
    point3 = pointDiagram' # named (Point 3)
    outNode = pointDiagram' # named Out
    between = strutX 4 # named Between
    underConstant = 4
    maxHeight = calcMaxHeight diagram1 diagram2
    middle = centerY (diagram1 ||| between ||| diagram2)
    matrix = placeBelowLeft (2*underConstant + maxHeight) underConstant Between outNode
           $ placeBelow (underConstant + maxHeight) Between point2
           $ placeBelow underConstant (N2 .> Out) point3
           $ placeBelow underConstant (N1 .> Out) point1
           $ placeAbove maxHeight Between inNode middle
    in center $ buildArrowsRightApp matrix

pointDiagram' :: Diagram SVG
pointDiagram' = pointDiagram origin

calcMaxHeight :: Diagram SVG -> Diagram SVG -> Double
calcMaxHeight d1 d2 = 2 + max (height d1) (height d2)/1.25

placeAbove,placeBelow :: IsName nm => Double -> nm -> Diagram SVG -> Diagram SVG -> Diagram SVG
placeAbove i n d1 = withName n $ \sub -> atop $ place d1 $ upper i sub
placeBelow i n d1 = withName n $ \sub -> atop $ place d1 $ lower i sub

placeBelowRight,placeBelowLeft :: IsName nm => Double -> Double -> nm -> Diagram SVG -> Diagram SVG -> Diagram SVG
placeBelowRight i j = placeDisplaced j (-i)
placeBelowLeft i j = placeDisplaced (-j) (-i)

upper,lower :: Double -> Subdiagram SVG V2 Double Any -> Point V2 Double
upper i = displace (0,i)
lower i = upper (-i)

placeDisplaced :: IsName nm => Double -> Double -> nm -> Diagram SVG -> Diagram SVG -> Diagram SVG
placeDisplaced i j n d1 = withName n $ \sub -> atop $ place d1 $ displace (i, j) sub

displace :: (Double, Double) -> Subdiagram SVG V2 Double Any -> Point V2 Double
displace (i,j) sub = location sub .+^ (i *^ unitX ^+^ j *^ unitY)

arrowOptions :: Dir -> ArrowOpts Double
arrowOptions dir = with & arrowShaft .~ getShaft dir & shaftStyle %~ lw thin & arrowHead .~ tri & headLength .~ thin

arrowOptions' :: Dir -> (Point V2 Double, Point V2 Double) -> ArrowOpts Double
arrowOptions' dir points = with & arrowShaft .~ (getShaftCubicSpline points dir) & shaftStyle %~ lw thin & arrowHead .~ tri & headLength .~ thin

simpleArrowOptions :: ArrowOpts Double
simpleArrowOptions = with & shaftStyle %~ lw thin & arrowHead .~ tri & headLength .~ thin

rename :: (IsName nm1, IsName nm2) => nm1 -> nm2 -> Diagram SVG -> Diagram SVG
rename n1 = nameSub (fromJust.lookupName n1)

getShaftCubicSpline :: (TrailLike t, V t ~ V2, N t ~ Double) => (Point (V t) Double, Point (V t) Double) -> Dir -> t
getShaftCubicSpline (p1, p2) L = cubicSpline False [p1, getMiddlePointLeft (p1, p2), p2]
getShaftCubicSpline (p1, p2) R = cubicSpline False [p1, getMiddlePointRight (p1, p2), p2]

pointX, pointY :: Point V2 Double
pointX = p2 (1.0, 3.0)
pointY = p2 (3.0, 6.0)

result :: (TrailLike t, V t ~ V2, N t ~ Double) => t
result = getShaftCubicSpline (pointX, pointY) L
-- getSingleCubicSpline p = cubicSpline False [p]

getMiddlePointLeft, getMiddlePointRight :: (Point V2 Double, Point V2 Double) -> Point V2 Double
getMiddlePointLeft (p1, p2) = let
  middle = p1 + 0.5 * (p2 - p1)
  in middle + 0.1 * getOrthogonalLeft middle
getMiddlePointRight (p1, p2) = let
  middle = p1 + 0.5 * (p2 - p1)
  in middle + 0.1 * getOrthogonalRight middle

getOrthogonalLeft, getOrthogonalRight :: Point V2 Double -> Point V2 Double
getOrthogonalLeft (P (V2 x y)) = P (V2 (-y) x)
getOrthogonalRight (P (V2 x y)) = P (V2 y (-x))

getShaft :: (TrailLike t, V t ~ V2) => Dir -> t
getShaft L = shaftL
getShaft R = shaftR

shaftL,shaftR :: (TrailLike t, V t ~ V2) => t
shaftL = arc xDir (70/360 @@ turn)
shaftR = arc xDir (-70/360 @@ turn)


maybeColorGrayTop, maybeColorGrayBottom :: MonConnective -> Diagram SVG -> Diagram SVG
maybeColorGrayTop c = maybeColorGray (not $ isResidual c)
maybeColorGrayBottom c = maybeColorGray (isResidual c)

maybeColorGray :: Bool -> Diagram SVG -> Diagram SVG
maybeColorGray True d = d # fcA transparentGray
maybeColorGray False d = d

transparentGray = gray `withOpacity` 0.4

buildArrowsLeftApp :: Diagram SVG -> Diagram SVG
buildArrowsLeftApp d = d
                    # drawArrow (arrowOptions L) (In, N1 .> In) curvedRightToBottom
                    # drawArrow (arrowOptions R) (In, N2 .> In) curvedLeftToBottom
                    # drawArrow simpleArrowOptions (N1 .> Out, Point 1) topToBottom
                    # drawArrow simpleArrowOptions (N2 .> Out, Point 3) topToBottom
                    # drawArrow (arrowOptions L) (Point 1, Point 2) straightTopToLeft
                    # connectPerim' (arrowOptions L) (Point 2) (Point 3) (0/360 @@ turn) (200/360 @@ turn)
                    # connectPerim' (arrowOptions R) (Point 3) Out (340/360 @@ turn) (90/360 @@ turn)

buildArrowsRightApp :: Diagram SVG -> Diagram SVG
buildArrowsRightApp d = d
                    # drawArrow (arrowOptions L) (In, N1 .> In) curvedRightToBottom
                    # drawArrow (arrowOptions R) (In, N2 .> In) curvedLeftToBottom
                    # drawArrow simpleArrowOptions (N1 .> Out, Point 1) topToBottom
                    # drawArrow simpleArrowOptions (N2 .> Out, Point 3) topToBottom
                    # drawArrow (arrowOptions R) (Point 3, Point 2) straightTopToRight
                    # connectPerim' (arrowOptions R) (Point 2) (Point 1) (180/360 @@ turn) (340/360 @@ turn)
                    # connectPerim' (arrowOptions L) (Point 1) Out (200/360 @@ turn) (90/360 @@ turn)

drawArrow :: (IsName n1, IsName n2) => ArrowOpts Double -> (n1, n2) -> (Angle Double, Angle Double) -> Diagram SVG -> Diagram SVG
drawArrow opts names = uncurry (uncurry (connectPerim' opts) names)

drawArrow' :: (IsName n1, IsName n2) => Dir -> (n1, n2) -> (Angle Double, Angle Double) -> Diagram SVG -> Diagram SVG
drawArrow' dir names = uncurry ((uncurry f names) dir)


f :: (IsName n1, IsName n2) => n1 -> n2 -> Dir -> Angle Double -> Angle Double -> Diagram SVG -> Diagram SVG
f n1 n2 dir a1 a2 = withName n1 $ \sub1 ->
                    withName n2 $ \sub2 ->
                  connectPerim' (arrowOptions' dir (location sub1, location sub2)) n1 n2 a1 a2

getArrows,getLeftArrows,getRightArrows :: Diagram SVG -> MonConnective -> Diagram SVG
getArrows d c = getLeftArrows (getRightArrows d c) c
getLeftArrows d c | leftRotate c = d
                    # drawArrow (arrowOptions R) (Out, N1 .> In) curvedLeftToTop
                    # drawArrow (arrowOptions R) (N1 .> Out, In) curvedBottomToRight
                  | otherwise    = d
                    # drawArrow' L (In, N1 .> In) curvedRightToBottom
                    # drawArrow (arrowOptions L) (N1 .> Out, Out) curvedTopToLeft
getRightArrows d c | rightRotate c = d
                    # drawArrow (arrowOptions L) (Out, N2 .> In) curvedRightToTop
                    # drawArrow (arrowOptions L) (N2 .> Out, In) curvedBottomToLeft
                   | otherwise     = d
                     # drawArrow (arrowOptions R) (In, N2 .> In) curvedLeftToBottom
                     # drawArrow (arrowOptions R) (N2 .> Out, Out) curvedTopToRight

maybeLeftRotate, maybeRightRotate :: MonConnective -> Diagram SVG -> Diagram SVG
maybeLeftRotate c d | leftRotate c = rotate (180/360 @@ turn) d
                    | otherwise    = d
maybeRightRotate c d | rightRotate c = rotate (180/360 @@ turn) d
                    | otherwise    = d

curvedTopToLeft, curvedTopToRight, curvedBottomToLeft, curvedBottomToRight :: Floating n => (Angle n, Angle n)
curvedTopToLeft = uniBimap makeAngle (270, 160)
curvedTopToRight = uniBimap makeAngle (270, 20)
curvedBottomToLeft = uniBimap makeAngle (90, 340)
curvedBottomToRight = uniBimap makeAngle (90, 200)

curvedLeftToTop, curvedRightToTop, curvedLeftToBottom, curvedRightToBottom :: Floating n => (Angle n, Angle n)
curvedLeftToTop = swap curvedTopToLeft
curvedRightToTop = swap curvedTopToRight
curvedLeftToBottom = swap curvedBottomToLeft
curvedRightToBottom = swap curvedBottomToRight

straightTopToLeft, straightTopToRight, straightBottomToLeft, straightBottomToRight :: Floating n => (Angle n, Angle n)
straightTopToLeft = uniBimap makeAngle (270, 180)
straightTopToRight = uniBimap makeAngle (270, 0)
straightBottomToLeft = uniBimap makeAngle (90, 0)
straightBottomToRight = uniBimap makeAngle (90, 180)

topToBottom, bottomToTop :: Floating n => (Angle n, Angle n)
topToBottom = uniBimap makeAngle (270, 90)
bottomToTop = swap topToBottom

makeAngle :: Floating n => n -> Angle n
makeAngle n = n/360 @@ turn

uniBimap :: Bifunctor p => (a -> b) -> p a a -> p b b
uniBimap f = bimap f f
