module Main(main) where
import Graphics.Gloss
import Data.Time
import Data.Time.LocalTime

window :: Display
window = InWindow "clock" (200, 200) (10, 10)

background :: Color
background = white

mkclock :: Float -> Float -> Float -> Picture
mkclock hr mn se = Pictures
  [ thickCircle rClock 20
  , hour hr
  , minute mn
  , m1
  , m2
  , m3
  , m4
  , m5
  , m6
  , m7
  , m8
  , m9
  , m10
  , m11
  , m12
  , mins
  , color red (sec se)
  , color red (thickCircle 4 8)
  ]
  where
  rClock = 300
  rHour  = 125
  rmin   = 225
  rsec   = 250
  o = (0,0)
  hour h = rotate (30 * h) (translate 0 30 (rectangleSolid 20 195))
  minute m = rotate (6 * m) (translate 0 80 (rectangleSolid 15 245))
  sec se = rotate (6 * se) (translate 0 50 (rectangleSolid 5 250))
  m1 =  (translate 0 250 (rectangleSolid 20 50))
  m2 = rotate 30 (translate 0 250 (rectangleSolid 20 50))
  m3 = rotate 60 (translate 0 250 (rectangleSolid 20 50))
  m4 = rotate 90 (translate 0 250 (rectangleSolid 20 50))
  m5 = rotate 120 (translate 0 250 (rectangleSolid 20 50))
  m6 = rotate 150 (translate 0 250 (rectangleSolid 20 50))
  m7 = rotate 180 (translate 0 250 (rectangleSolid 20 50))
  m8 = rotate 210 (translate 0 250 (rectangleSolid 20 50))
  m9 = rotate 240 (translate 0 250 (rectangleSolid 20 50))
  m10 = rotate 270 (translate 0 250 (rectangleSolid 20 50))
  m11 = rotate 300 (translate 0 250 (rectangleSolid 20 50))
  m12 = rotate 330 (translate 0 250 (rectangleSolid 20 50))
 {- s1  = rotate 6 (translate 0 250 (rectangleSolid 10 25))
  s2  = rotate 12 (translate 0 250 (rectangleSolid 10 25))
  s3  = rotate 18 (translate 0 250 (rectangleSolid 10 25))
  s4  = rotate 24 (translate 0 250 (rectangleSolid 10 25))
  s5  = rotate 30 (translate 0 250 (rectangleSolid 10 25))
  -}
  mins = pictures $ map (\angle -> rotate angle (translate 0 270 (rectangleSolid 10 25))) [0,6..360]
main :: IO ()
main = do
    z <- getCurrentTimeZone
    t <- getCurrentTime
    let l = utcToLocalTime z t
    animate window white (mkpicture l)

mkpicture :: LocalTime -> Float -> Picture
mkpicture lt x =
    let time = addLocalTime (fromInteger (round x)) lt
        h = todHour $ localTimeOfDay time
        m = todMin $ localTimeOfDay time
        ps = todSec $ localTimeOfDay time
        s = fromIntegral $ round ps
        in (mkclock (fromIntegral h) (fromIntegral m) s)
