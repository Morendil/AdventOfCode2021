import Test.Hspec
import Data.List.HT

highest :: (Int, Int) -> (Int, Int) -> Int
highest (xmin, xmax) (ymin, ymax) = (abs ymin * (abs ymin-1)) `div` 2

step :: ((Int,Int), (Int, Int)) -> ((Int,Int), (Int, Int))
step ((vx,vy),(px,py)) = ((max 0 (vx-1), vy-1),(px+vx,py+vy))

simulate :: ((Int,Int), (Int, Int)) -> (Int, Int) -> Bool
simulate ((xmin,xmax),(ymin,ymax)) (vx,vy) = any hitsTarget steps
  where hitsTarget (_,(x,y)) = x >= xmin && x <= xmax && y >= ymin && y <= ymax
        goneForever (_,(x,y)) = x > xmax || y < ymin
        steps = takeUntil goneForever $ iterate step ((vx,vy),(0,0))

main = do
    -- part1
    print $ highest (195,238) (-93,-67)
    -- part 2
    print $ length $ filter id [simulate ((195,238),(-93,-67)) (vx,vy)| vx<-[19..238], vy<-[-93..92]]

test :: IO ()
test = hspec $ do
  describe "Trajectory" $ do
    it "gets the example right" $
      highest (20,30) (-10,-5) `shouldBe` 45
