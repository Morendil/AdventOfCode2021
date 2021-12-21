import Data.Tuple.Extra (first, second)
import Data.List.HT

data Player = Player {pos:: Int, score:: Int}
      deriving (Eq, Show)
data Game = Game {die:: Int, rolls::Int, current::Int, players::(Player, Player)}
      deriving (Eq, Show)
advance limit by current = ((current-1+by) `mod` limit)+1

step :: Game -> Game
step Game {die=die, rolls=rolls, current=current, players=players} = Game {die=die', current=current', players=players', rolls=rolls+3}
    where rollsTaken = take 3 $ iterate (advance 100 1) die
          die' = advance 100 1 $ last rollsTaken
          current' = advance 2 1 current
          players' = which update players
          update Player {pos=pos, score=score} = Player{pos=pos', score=score'}
            where pos'=advance 10 (sum rollsTaken) pos
                  score'=score+pos'
          which = if current == 1 then first else second

won :: Game -> Bool
won Game {players=(p1,p2)} = any ((>=1000) . score) [p1,p2]

main = do
      let initial = Game {die=1, current=1, players=(Player{pos=4,score=0},Player{pos=10,score=0}),rolls=0}
      let Game {rolls=rolls, players=(p1,p2)} = last $ takeUntil won $ iterate step initial
      print $ minimum (map score [p1,p2]) * rolls
