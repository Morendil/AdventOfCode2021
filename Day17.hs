import Test.Hspec

highest :: (Int, Int) -> (Int, Int) -> Int
highest (xmin, xmax) (ymin, ymax) = (abs ymin * (abs ymin-1)) `div` 2

step :: ((Int,Int), (Int, Int)) -> ((Int,Int), (Int, Int))
step ((vx,vy),(px,py)) = ((max 0 (vx-1), vy-1),(px+vx,py+vy))

main = do
    -- part1
    print $ highest (195,238) (-93,-67)

test :: IO ()
test = hspec $ do
  describe "Trajectory" $ do
    it "gets the example right" $
      highest (20,30) (-10,-5) `shouldBe` 45
