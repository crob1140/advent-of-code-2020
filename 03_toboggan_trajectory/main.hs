countTreesInPath :: Int -> Int -> [[Char]] -> Int
countTreesInPath _ _ [] = 0
countTreesInPath dx dy grid = countTreesInPath' dx dy
    where
        width = length (head grid)
        height = length grid
        countTreesInPath' x y
            | y >= height = 0                                                                            -- Base case: we've reached the bottom
            | x >= width = countTreesInPath' (x - width) y                                               -- When we reach the right-side, just wrap around since the pattern repeats
            | otherwise = (fromEnum (((grid !! y) !! x) == '#')) + countTreesInPath' (x + dx) (y + dy)   -- Otherwise, check the character at this index, and then call this function again with the next index to keep moving down
                                                                                                         -- TODO: This does not scale well at all and could use some performance improvements. Lists in Haskell are linked lists, so checking each value requires us to iterate from the top-left again. 
solvePart1 :: [[Char]] -> Int
solvePart1 = countTreesInPath 3 1

solvePart2 :: [[Char]] -> Int
solvePart2 grid = product treesInPaths
    where
         treesInPaths = map (\(dx,dy) -> countTreesInPath dx dy grid) [(1,1),(3,1),(5,1),(7,1),(1,2)]

main = putStrLn ("Answer 1: " ++ (show answer1) ++ "\n" ++ "Answer 2: " ++ (show answer2))
    where
        grid = lines input
        answer1 = solvePart1 grid
        answer2 = solvePart2 grid

-- Putting the input into a string variable rather than a seperate file so that the script can be copied and pasted into an online compiler.
input = "....#...##.#.........#....#....\n\
\#.......#...#...#.#............\n\
\#..#..#.#.##....#.#........#...\n\
\........##...................#.\n\
\........#...##...#.#.###.......\n\
\##............#...#.....#.##...\n\
\...........#....###...#.....#..\n\
\.......#......#..##..#.....#...\n\
\..#.#..#....#.........#...#..#.\n\
\.........##......#.....##..##..\n\
\........#.....#....#..##......#\n\
\..#..#.......#..............#..\n\
\.....#.#.......................\n\
\.#.#....#.........#............\n\
\.......#.....#.###.............\n\
\......##......#...........#..#.\n\
\.#...............##...#........\n\
\.....#..##........###.........#\n\
\#...........#..#............#..\n\
\.........#....#..#.#......#....\n\
\.......#.........#..##.........\n\
\.##.....#..................#...\n\
\....#............#.#....#.....#\n\
\..#....#...##....#...#.#...#...\n\
\..........#................#.#.\n\
\#...#.#.#.####..#.#..........#.\n\
\..#...#.##......#...........#..\n\
\..#.....#...#.......#......#..#\n\
\..............#.......#........\n\
\.#..#..........#.....#...#..#.#\n\
\#........#...#......#.......#..\n\
\#..................#...........\n\
\..#...#........#...#..#........\n\
\..............#.....#.....#..#.\n\
\#.#.......#..............##.##.\n\
\....#.#.....##....#...........#\n\
\......#....#...#..#.......#....\n\
\....#..#.#.....#..##.....#....#\n\
\...........#.......#.#.#.......\n\
\#.......#..##........#..#......\n\
\.........#.##..#..............#\n\
\...........#............###.#..\n\
\..#.....#.....##...#.........#.\n\
\....##............##........#..\n\
\.....###..........#......##....\n\
\#...##..#..#..........#........\n\
\....#.....#.......#..#.#...##..\n\
\.#....#........#.#.........#.#.\n\
\##...#.#.....#......#..........\n\
\.....##.....#....#.....###.#..#\n\
\..............#..###..#...#..#.\n\
\....#...#....#.............#.#.\n\
\.#.........#.....#........#.##.\n\
\....#.........#..........#.....\n\
\.......#........#.#.#..........\n\
\#........##....#.........#.....\n\
\..##..........#....#.#...#....#\n\
\#...#.#......#..##..........#.#\n\
\.....#..#...#..#...............\n\
\#...#..............#...........\n\
\.#...#....#..##.....#....#.#...\n\
\.#...#.......#...#..#.##....#..\n\
\#....#........#....#...#.......\n\
\#..#......#.....#.....#..##....\n\
\......#.#....##....##..#...#...\n\
\..#....#.#.###..............#..\n\
\.#.##.......#.#.#..#...#..#....\n\
\..#..........#.#....#..#.#....#\n\
\..........#...#...#..........#.\n\
\..........#.....#.#..#..#....##\n\
\.#.#...##...#...........####...\n\
\........##..#.#..........#.##.#\n\
\#......###...........#...#.....\n\
\..#.#....##.........##....#....\n\
\#....#.##..##..#..#.....#.....#\n\
\.##.....##....##....#.......#..\n\
\#...#.....##....#..........#...\n\
\............#.#.##....#....#...\n\
\....#............#.....#......#\n\
\....................#..........\n\
\..#....................#..#....\n\
\....#.....#........#..##...#...\n\
\#.....#.#....................##\n\
\.#....#.#.#...#..........#....#\n\
\....#...#......#...#.....##...#\n\
\.....#.........................\n\
\.......#..#.#...#...#...#.....#\n\
\...#......#.##.#...#..#...##.#.\n\
\...........................#..#\n\
\..#.#.....#........##..........\n\
\....#...##........#.#.#..#...##\n\
\..##.....#..###.........##.##..\n\
\.#..#.....#...#.............#..\n\
\#..............##...#....##....\n\
\.##......#.#............#......\n\
\.............##...#.#.......#..\n\
\.........#..#..#...............\n\
\........##......#....##........\n\
\...#.........#.#.#.............\n\
\#..........#......#......#..#..\n\
\.............##.#.#..#.#.#...#.\n\
\.....#.........#...............\n\
\..##.#..#.....##..#........#.#.\n\
\.#..........#.#.......#......##\n\
\.#........................#....\n\
\#....#....#...#..#......#......\n\
\........#.......#......#.....#.\n\
\.....#....##..#...###...#....#.\n\
\....#.........#....#......#....\n\
\.............#...#....#.......#\n\
\.....#.........#..#.#..........\n\
\.........#..#........#.#.#.....\n\
\......#.##......#....#.#.##.#..\n\
\.#...#.#...#...#.#......#....##\n\
\.#................#......#.....\n\
\#.#.#...............#..........\n\
\.....#.#.......#...#........#..\n\
\#...#.#.#.##..#...........#..#.\n\
\.............###.........#....#\n\
\.#.....#.......##....##.......#\n\
\....#...#.......#.##.....#.....\n\
\...........##.........#...#....\n\
\..............#.#..#.....#..#..\n\
\#.#...#..#.#.........#......#.#\n\
\#.##.....##....#........#.#.#.#\n\
\##.#.###.........##.......#..#.\n\
\#.....#.....................#..\n\
\.........##........#...........\n\
\.###........##....#...#........\n\
\....#.#........##...........#..\n\
\..........#.....#..........#..#\n\
\......#..............#......#..\n\
\.....#...#......#...#...#......\n\
\..........#.#..#....#...#..###.\n\
\#..##........#................#\n\
\..#............................\n\
\.....#.........#.#.............\n\
\........#...#.....#...##......#\n\
\..#........#................#..\n\
\......#....#..#......#.........\n\
\...........##....#..#.#........\n\
\.....#.............###.........\n\
\#............#......#..#.......\n\
\..#..#.................#..#..##\n\
\.......#......#.....#........#.\n\
\....................#..#.##...#\n\
\.#..##...............##...#....\n\
\...#...#....#........#.........\n\
\.....##...#.....###............\n\
\.###.........#........#.....##.\n\
\.............#...#.............\n\
\...#.#...............#..##..#.#\n\
\...#...............#..#.....#..\n\
\....#.#..................#...#.\n\
\..........#...........#.#...###\n\
\#...#......#................#..\n\
\...#.#.......#...#......#.##...\n\
\......#..........#.............\n\
\##.......#.##.#...........#....\n\
\......#...#.#.....#............\n\
\.#.....#.....#.....#.........#.\n\
\..................#............\n\
\.#.#.#.....#......#.##.........\n\
\.......#..##.##......#..#....#.\n\
\...#.#.#......#...#........#...\n\
\..#............#......#.......#\n\
\..#......#........#.........#..\n\
\..#..#.#.#.....#.............#.\n\
\..#.#..##......#...#...##......\n\
\.##...#....##.#.#...........#..\n\
\..............#..#...#....#....\n\
\.......#.#........#............\n\
\.....##..###........#..........\n\
\......................#........\n\
\..##....#....#.................\n\
\.##.#.###.#........#.##..#...#.\n\
\##................#...........#\n\
\....#..##.....##...............\n\
\.#.....#..#............#.....#.\n\
\#.........#..............#.....\n\
\...##.#......#...#.............\n\
\................#..............\n\
\...#.....#....##...#..#....#...\n\
\..............##..#...#.##..#..\n\
\......................#..#....#\n\
\.......#....#..#.##.........#.#\n\
\#...#........##.......#........\n\
\...##...............#.....#....\n\
\.##...##...#...................\n\
\.........##.#...#.........#....\n\
\............#............#..#..\n\
\.............................#.\n\
\....#.#....#...................\n\
\......#......#...#..##.........\n\
\#........#.#.#.#.#......#....#.\n\
\.#.........#.#...#......#..#.#.\n\
\..............#....##.........#\n\
\.#.......#..#....#.#.#....#....\n\
\...###.#.#..#...#....#....#....\n\
\#........#....................#\n\
\......#...##.###..#..##...#....\n\
\.....#........#.......#........\n\
\#..#...........#.#.............\n\
\....##.#...#..##............##.\n\
\#.#..##..#...#...#.....#.......\n\
\..#.............#.##..#...#.##.\n\
\.#.....##.#..#...#...........#.\n\
\....#...#....................##\n\
\....##......#.###......#......#\n\
\...#...#.........#..#.##....#..\n\
\#......#..#....###.........#...\n\
\#...........##.............#.#.\n\
\#..............##....#......#..\n\
\.........#...#.#...#...#.......\n\
\....#....#............#.......#\n\
\........#...#....#......##.....\n\
\..........#.#..#.........#.....\n\
\#........#.##....##......#.....\n\
\...#.......#...................\n\
\###...#...#..#.##....#.....#...\n\
\........##..........#.##..#....\n\
\.....#......#..#.....#.....#.#.\n\
\...#..#..##..###.....##.#......\n\
\#..#......##...#............#..\n\
\#............#....#..#.........\n\
\#........#.......#......#..##.#\n\
\...#.#.........#.#.............\n\
\#..............#..............#\n\
\#.#......#..........##.........\n\
\#..##...........#..##...#......\n\
\.....#.#.....#......#.....#.#.#\n\
\.#.##...#...##...........#....#\n\
\#.............#........#.......\n\
\..##.............#...#.........\n\
\....#.#......###....#..........\n\
\...#..#.....#..##.#....#...#.#.\n\
\.............##................\n\
\#.#............#........#..#.#.\n\
\.#......#.....#...........#....\n\
\...#.........#...........#.##..\n\
\.....#...#.....#..#..........#.\n\
\........#.#...............#.#..\n\
\.......#..#..#.....#.......##..\n\
\.#...#...#..#...##...#.........\n\
\..........##....#..#.##..#.....\n\
\....#.................#...#....\n\
\.........#...#......#....#....#\n\
\.........#..#...#.##........##.\n\
\#.#....##.......#.#............\n\
\##.......##..................#.\n\
\......#...#......##............\n\
\##.#...#.#...........#..#......\n\
\.........#.........#..#.#...#..\n\
\.#...#.......#.#...###.........\n\
\................#.#.....#......\n\
\..#...#.....#........#.........\n\
\.........##.###.#.#.....#...#..\n\
\#..#..........#....#.#...#...##\n\
\##.#.#....#..##.............#.#\n\
\.###....#..#...............##..\n\
\............#......#.#.#....#..\n\
\........#...#..#...#...........\n\
\##.........#................#..\n\
\...###...#.#..#...#..........##\n\
\...#......#......##........#...\n\
\.......#............#..........\n\
\.....#.....##....#.....###.....\n\
\.#...#...#.....#..#..#....#..#.\n\
\#.#........#..#.......##...#.##\n\
\.....#.....##..#.##........#..#\n\
\.....#...#...........#.........\n\
\..#....#.#...#..#....##...#...#\n\
\...........#...##.........#....\n\
\..#....#....##........#.####...\n\
\#.............#.#.............#\n\
\...................#.....#.#..#\n\
\.#....#.#.............#.#......\n\
\#...........#............#.#...\n\
\..#.........#.#....#.......##..\n\
\#....####......#...#......#....\n\
\....##....#...................#\n\
\....#.##....#.............#....\n\
\.........##........#.....#..#..\n\
\............#...#..............\n\
\............#..##....#.....##.#\n\
\............#.....#......#.....\n\
\........#..#........##.#.......\n\
\...#.#........#..............#.\n\
\............#.........#..#.#...\n\
\................#.............#\n\
\..##..........##......#.#......\n\
\..#..#.##....#.........#...#...\n\
\...........##...#.#.#..........\n\
\.#.#.......#.#...#.........#...\n\
\.........#..#........#..#.#....\n\
\..........##..#.##....#....#...\n\
\....#...............#.......#..\n\
\##..........##.................\n\
\....#.#.#.....#..........##.#..\n\
\..............#.##..........##.\n\
\##...............#...#..#......\n\
\..#..#..........#......#.......\n\
\#...#..##.#.#..................\n\
\....#....##......##.#...#....##\n\
\.#...#.#....##.............#..#\n\
\................#......###.....\n\
\..#..#.............#.#.......#.\n\
\..#..................#.......#.\n\
\.....#.......#....#.##...#.##..\n\
\.....##.......#......#..#......\n\
\#..#.......#........#..........\n\
\..#...#..#....#.........#......\n\
\#..#..#......##..#.##....####..\n\
\......##.#.....#..#.......#....\n\
\.##...#.....#..#...#.#.........\n\
\#.....#........###....#...#..#.\n\
\.#....#.#..#......#............\n\
\.........#..#..#.....#........#\n\
\..#.......#..........#..#......\n\
\......#.......##.#....#.#.#....\n\
\.#............#.....#.......#..\n\
\...#..#...............#........\n\
\.....#.........................\n"