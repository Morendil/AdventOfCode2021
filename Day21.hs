import Data.Tuple.Extra (first, second, both)
import Data.List.HT
import qualified Data.Map as M

type State = ((Int, Int), (Int, Int))
type Universes = M.Map State Integer

forks :: [(Integer, Int)]
forks = [(1,3),(3,4),(6,5),(7,6),(6,7),(3,8),(1,9)]

stepAll :: (Int, Universes) -> (Int, Universes)
stepAll (current, universes) = (current', foldr (evolve current universes) M.empty forks)
      where current' = advance 2 1 current

evolve :: Int -> Universes -> (Integer, Int) -> Universes -> Universes
evolve current oldMap (weight, rolls) newMap = foldr forkOne newMap (M.toList oldMap)
      where forkOne (state, stateWeight) aMap | isWon state = M.insert state stateWeight aMap
            forkOne (state, stateWeight) aMap = M.alter adjustOne state' aMap
                  where state' = which update state
                        which = if current == 1 then first else second
                        update (pos, score) = let pos' = advance 10 rolls pos in (pos', score+pos')
                        adjustOne Nothing = Just $ weight * stateWeight
                        adjustOne (Just oldWeight) = Just $ oldWeight + weight * stateWeight

isWon :: State -> Bool
isWon ((_,score1),(_,score2)) = score1 >= 21 || score2 >= 21

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
      let initial = Game {die=1, current=1, players=(Player{pos=4,score=0},Player{pos=8,score=0}),rolls=0}
      let Game {rolls=rolls, players=(p1,p2)} = last $ takeUntil won $ iterate step initial
      print $ minimum (map score [p1,p2]) * rolls
      -- 444356092776315 universes, while player 2 merely wins in 341960390180808
      let initUniverse = M.insert ((4,0),(8,0)) 1 M.empty
          -- takeUntil (\(current,universes) -> all isWon $ M.keys universes)
          allUniverses = last $ takeUntil (\(current,universes) -> all isWon $ M.keys universes) $ iterate stepAll (1, initUniverse)
          scores = M.partitionWithKey (\((_,s1),(_,_)) _ -> s1 >= 21) $ snd allUniverses
      print $ both (sum . M.elems) scores