module Main (main) where
import Graphics.Gloss
import Data.Massiv.Core
import qualified Data.Massiv.Array as M
import System.Random
import Data.List.Split
import Control.Monad
import Data.Maybe

data State = State
    { world :: M.Matrix M.B Bool
    ,t :: Int
    }

size :: Int
size = 250 

squareSize :: Float 
squareSize = 10 

lifeRules :: Bool -> Int -> Bool
lifeRules True 2  = True
lifeRules True 3  = True
lifeRules False 3 = True
lifeRules _ _     = False

rolls :: Int -> IO [Bool]
rolls x = replicateM (x*x) randomIO 

toList :: [[Bool]] -> Maybe (M.Matrix M.B Bool)
toList = M.fromListsM M.Seq 

populate :: Int -> IO (M.Matrix M.B Bool)
populate x = do fromJust . toList . chunksOf x <$> rolls x

multiplyWorld :: M.Matrix M.B Bool -> M.Matrix M.B Bool
multiplyWorld = multiplyWorldY . multiplyWorldX 
    where multiplyWorldX :: M.Matrix M.B Bool -> M.Matrix M.B Bool -- flip and copy the world along the x axis
          multiplyWorldX x = M.compute $ M.append' 2 x (M.reverse Dim2 x)
          multiplyWorldY :: M.Matrix M.B Bool -> M.Matrix M.B Bool -- flip and copy the world again along the y axis
          multiplyWorldY x = M.compute $ M.append' 2 x' (M.reverse Dim2 x')
            where x' = M.transpose x


hueToRGB :: Float -> Float -> Float -> Float
hueToRGB p q t 
    | t' < 1/6  = p + (q - p) * 6 * t'
    | t' < 1/2  = q
    | t' < 2/3  = p + (q - p) * (2/3 - t') * 6
    | otherwise = p
    where t'
            | t < 0 = t + 1
            | t > 1 = t - 1
            | otherwise = t

rainbow :: Float -> Float -> Float -> Color
rainbow h s l = makeColor r g b 1
        where q = if l < 0.5 then l * (1 + s) else l + s - l * s
              p = 2 * l - q
              r = hueToRGB p q (h + 1/3)
              g = hueToRGB p q h
              b = hueToRGB p q (h - 1/3)

render :: State -> Picture
render s = pictures 
        $ concat 
        $ M.toList 
        $ M.imap (\(i :. j) x -> [
                Translate
                (fromIntegral i*squareSize - fromIntegral size * squareSize / 2) (fromIntegral j*squareSize - fromIntegral size*squareSize/2)
                $ if x 
                  then Color (rainbow (fromIntegral ((i+j+offset) `mod` 360) / 360) 0.65 0.7) $ Polygon [(0,0), (0,squareSize), (squareSize,squareSize), (squareSize,0)] 
                  else Color (rainbow (fromIntegral ((i+j+offset) `mod` 360) / 360) 0.5 0.45) $ Line [(0,0), (0,squareSize), (squareSize,squareSize), (squareSize,0)]  
                ])
        $ world s
        where offset = t s

lifeStencil :: M.Stencil Ix2 Bool Bool
lifeStencil = M.makeStencil (Sz (3 :. 3)) (1 :. 1) $ \get -> 
        lifeRules (get (0 :. 0)) $ length $ filter id [
            get (-1 :. -1), get (-1 :. 0), get (-1 :. 1),
            get ( 0 :. -1),                get ( 0 :. 1),
            get ( 1 :. -1), get ( 1 :. 0), get ( 1 :. 1) 
        ]

nextFrame :: a -> b -> State -> State
nextFrame _ _ s = s { world = newWorld, t = t'}
        where newWorld = M.compute $ M.mapStencil M.Wrap lifeStencil (world s)
              t' = t s + 1

main :: IO ()
main = do
        world' <- populate size
        let worldState = State world' 0
        simulate FullScreen (makeColorI 18 18 18 1) 30 worldState render nextFrame
