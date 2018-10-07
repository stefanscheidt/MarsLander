import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let surfacen = read input_line :: Int -- the number of points used to draw the surface of Mars.
    
    surface <- replicateM surfacen $ do
        input_line <- getLine
        let input = words input_line
        let landx = read (input!!0) :: Int -- X coordinate of a surface point. (0 to 6999)
        let landy = read (input!!1) :: Int -- Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
        return (landx, landy)
    
    let landingArea = plateau surface
    hPutStrLn stderr (show landingArea)
    
    -- game loop
    forever $ do
        input_line <- getLine
        let input = words input_line
        let x = read (input!!0) :: Int
        let y = read (input!!1) :: Int
        let hspeed = read (input!!2) :: Int -- the horizontal speed (in m/s), can be negative.
        let vspeed = read (input!!3) :: Int -- the vertical speed (in m/s), can be negative.
        let fuel = read (input!!4) :: Int -- the quantity of remaining fuel in liters.
        let rotate = read (input!!5) :: Int -- the rotation angle in degrees (-90 to 90).
        let power = read (input!!6) :: Int -- the thrust power (0 to 4).
        
        -- hPutStrLn stderr "Debug messages..."

        let tele = Tele x y hspeed vspeed
        hPutStrLn stderr (show tele)
        let st = status landingArea tele
        hPutStrLn stderr (show st)
        let next = nextParams' st
        hPutStrLn stderr (show next)

        -- rotate power. rotate is the desired rotation angle. power is the desired thrust power.
        
        putStrLn $ printParams next


data Plateau = Plateau
  Int -- x1
  Int -- x2
  Int -- y
  deriving Show

toInt :: [(String, String)] -> [(Int, Int)]
toInt = map (\(x, y) -> (read x, read y))

plateau :: [(Int, Int)] -> Plateau
plateau surface = Plateau x1 x2 y
  where
    sameY ((_, y1), (_, y2)) = y1 == y2
    (points1, points2) = unzip $ filter sameY $ zip surface (tail surface)
    x1 = fst $ head points1
    x2 = fst $ head points2
    y = snd $ head points2


data Tele = Tele
  Int -- x
  Int -- y
  Int -- hs
  Int -- vs
  deriving Show

data Status
  = ToFarLeft (Int, Int)
  | ToFarRight (Int, Int)
  | FallingToFast (Int, Int)
  | FallingToSlow
  deriving Show

status :: Plateau -> Tele -> Status
status (Plateau lx1 lx2 ly) (Tele tx ty ths tvs) 
  | tx < lx1 = ToFarLeft (ths, tvs)
  | tx > lx2 = ToFarRight (ths, tvs)
  | abs ths > 80 = FallingToFast (ths, tvs)
  | otherwise = FallingToSlow

nextParams :: Status -> (Int, Int)
nextParams s = case s of
  ToFarLeft (hs, vs)  -> if hs < 0 then stop (hs, vs) else goRight 4
  ToFarRight (hs, vs) -> if hs > 0 then stop (hs, vs) else goLeft 4
  FallingToFast s     -> stop s  
  FallingToSlow       -> (0, 2)

nextParams' :: Status -> (Int, Int)
nextParams' s = case s of
  ToFarLeft (hs, vs)  -> goRight 4
  ToFarRight (hs, vs) -> goLeft 4
  FallingToFast s     -> stop s  
  FallingToSlow       -> (0, 2)


printParams :: (Int, Int) -> String
printParams (r, p) = (show r) ++ " " ++ (show p)

toDeg :: Double -> Int
toDeg x = round (x * (180.0 / 3.1416))

rotLeft :: Int -> Int
rotLeft p =  toDeg $ rad
  where
    p'  = fromIntegral p
    rad = acos (3.711 / p')

rotRight :: Int -> Int
rotRight p = (-1) * rotLeft p

goLeft :: Int -> (Int, Int)
goLeft p = (rotLeft p, p)

goRight :: Int -> (Int, Int)
goRight p = (rotRight p, p)

rotStop :: (Int, Int) -> Int
rotStop (hs, vs) = toDeg $ rad
  where
    hs' = fromIntegral hs
    vs' = fromIntegral hs
    s'  = sqrt (hs' * hs' + vs' * vs')
    rad = asin (hs' / s')

stop :: (Int, Int) -> (Int, Int)
stop (hs, vs) = (rotStop (hs, vs), 4)