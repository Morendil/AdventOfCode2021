import Data.Tuple.Extra (first, second, both)
import Data.List
import Data.List.HT (takeUntil )
import qualified Data.Map as M
import Data.Ord

type State = ((Int, Int), (Int, Int))
type Universe = (Integer, State)
type Fork = (Integer, Int)

forks :: [Fork]
forks = [(1,3),(3,4),(6,5),(7,6),(6,7),(3,8),(1,9)]

evolve :: (Int,[Universe]) -> (Int, [Universe])
evolve (player, universes) = (advance 2 1 player, merge $ forkAll player universes)

merge :: [Universe] -> [Universe]
merge universes = map mergeOne $ groupBy (\a b -> snd a == snd b) $ sortBy (comparing snd) universes
      where mergeOne items = (sum $ map fst items, snd $ head items)

forkAll :: Int -> [Universe] -> [Universe]
forkAll player = concatMap (forkEach player)

forkEach :: Int -> Universe -> [Universe]
forkEach player (weight, state) | isWon state = [(weight, state)]
forkEach player universe = map (forkOne player universe) forks

forkOne :: Int -> Universe -> Fork -> Universe
forkOne player (weight, state) (paths, rolls) = (weight * paths, state')
      where state' = which player update state
            update (pos, score) = let pos' = advance 10 rolls pos in (pos', score+pos')

isWon :: State -> Bool
isWon ((_,score1),(_,score2)) = score1 >= 21 || score2 >= 21

winner1 :: State -> Bool
winner1 ((_,score1),_) = score1 >= 21

advance limit by current = ((current-1+by) `mod` limit)+1
which player = if player == 1 then first else second

data Player = Player {pos:: Int, score:: Int}
      deriving (Eq, Show)
data Game = Game {die:: Int, rolls::Int, current::Int, players::(Player, Player)}
      deriving (Eq, Show)

step :: Game -> Game
step Game {die=die, rolls=rolls, current=current, players=players} = Game {die=die', current=current', players=players', rolls=rolls+3}
    where rollsTaken = take 3 $ iterate (advance 100 1) die
          die' = advance 100 1 $ last rollsTaken
          current' = advance 2 1 current
          players' = which current update players
          update Player {pos=pos, score=score} = Player{pos=pos', score=score'}
            where pos'=advance 10 (sum rollsTaken) pos
                  score'=score+pos'

won :: Game -> Bool
won Game {players=(p1,p2)} = any ((>=1000) . score) [p1,p2]

main = do
      let initial = Game {die=1, current=1, players=(Player{pos=4,score=0},Player{pos=8,score=0}),rolls=0}
      let Game {rolls=rolls, players=(p1,p2)} = last $ takeUntil won $ iterate step initial
      print $ minimum (map score [p1,p2]) * rolls
      -- 444356092776315 universes, while player 2 merely wins in 341960390180808
      let initial :: [Universe]
          initial = [(1,((4,0),(8,0)))]
          final = last $ takeUntil (all(isWon.snd).snd) $ iterate evolve (1,[(1,((4,0),(8,0)))])
      print $ both (sum.map fst) $ partition (winner1.snd) $ snd final
