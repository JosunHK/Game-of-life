module Main (main) where
import Graphics.Gloss
import Data.Massiv.Core
import qualified Data.Massiv.Array as M
import System.Random
import Data.List.Split
import Control.Monad
import Data.Maybe

size :: Int
size = 300 

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

render' :: M.Matrix M.B Bool -> Picture
render' = pictures . concat . M.toList . M.imap (\(i :. j) x -> [Translate (fromIntegral i*squareSize - fromIntegral size*squareSize/2) (fromIntegral j*squareSize - fromIntegral size*squareSize/2) $ if x then Color black $ Polygon [(0,0), (0,squareSize), (squareSize,squareSize), (squareSize,0)] else Blank])

lifeStencil :: M.Stencil Ix2 Bool Bool
lifeStencil = M.makeStencil (Sz (3 :. 3)) (1 :. 1) $ \get -> 
        lifeRules (get (0 :. 0)) $ length $ filter id [
            get (-1 :. -1), get (-1 :. 0), get (-1 :. 1),
            get ( 0 :. -1),                get ( 0 :. 1),
            get ( 1 :. -1), get ( 1 :. 0), get ( 1 :. 1) 
        ]

nextFrame :: a -> b -> M.Matrix M.B Bool -> M.Matrix M.B Bool
nextFrame _ _ = M.compute . M.mapStencil M.Wrap lifeStencil

main :: IO ()
main = do
        world <- populate size
        simulate FullScreen (makeColorI 58 58 58 1) 60 world render' nextFrame
