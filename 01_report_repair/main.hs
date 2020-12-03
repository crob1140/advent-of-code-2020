
findSummingGroup :: Int -> Int -> [Int] -> Maybe [Int]
findSummingGroup groupSize sum [] = Nothing                  -- If we've reached the end of the list and still haven't found a group, then there is no solution

findSummingGroup 1 sum xs =
    if elem sum xs then Just [sum]                          -- If we're only looking for one value and it exists, then return it as a singleton list
    else Nothing

findSummingGroup groupSize sum (x:xs) = 
    case findSummingGroup (groupSize - 1) remainder xs of   -- Search the remainder of the list for a group that is 1 smaller that sums to equal the remainder
        Just xs -> Just (x : xs)                            -- If this smaller group exists, then this element is part of the larger group and we can add it to the result
        Nothing -> findSummingGroup groupSize sum xs        -- If the smaller group does not exist, move along to the other entries
    where remainder = sum - x

main = putStrLn ("Answer 1: " ++ answer1 ++ "\n" ++ "Answer 2: " ++ answer2)
    where
        expenses = map read (lines input)
        answerForGroupSize groupSize = case findSummingGroup groupSize 2020 expenses of
            Just xs -> (show (product xs))
            Nothing -> "There is no solution"
        answer1 = answerForGroupSize 2
        answer2 = answerForGroupSize 3

input = "1384\n\
\1396\n\
\1072\n\
\1903\n\
\1387\n\
\1763\n\
\1600\n\
\1862\n\
\1992\n\
\1585\n\
\1909\n\
\1352\n\
\1288\n\
\1910\n\
\1070\n\
\1421\n\
\1802\n\
\1669\n\
\1059\n\
\1235\n\
\1854\n\
\1722\n\
\1275\n\
\198\n\
\1476\n\
\1588\n\
\1708\n\
\1217\n\
\1596\n\
\1355\n\
\1566\n\
\1973\n\
\1335\n\
\1480\n\
\1115\n\
\1272\n\
\1998\n\
\1821\n\
\2007\n\
\1721\n\
\1885\n\
\1420\n\
\1412\n\
\1487\n\
\1941\n\
\1835\n\
\1558\n\
\1061\n\
\1582\n\
\1940\n\
\1942\n\
\1210\n\
\1350\n\
\1175\n\
\1047\n\
\1456\n\
\1548\n\
\1110\n\
\1510\n\
\1995\n\
\1644\n\
\1968\n\
\1297\n\
\1198\n\
\1471\n\
\1360\n\
\1363\n\
\1528\n\
\1393\n\
\1365\n\
\1837\n\
\1886\n\
\2001\n\
\1161\n\
\1349\n\
\1787\n\
\988\n\
\1331\n\
\1960\n\
\1607\n\
\1324\n\
\97\n\
\1986\n\
\1955\n\
\1773\n\
\1443\n\
\1852\n\
\1368\n\
\1050\n\
\1378\n\
\1239\n\
\1750\n\
\1868\n\
\816\n\
\1965\n\
\1661\n\
\1728\n\
\1981\n\
\984\n\
\1037\n\
\1525\n\
\1789\n\
\1318\n\
\1952\n\
\1359\n\
\1358\n\
\1869\n\
\1641\n\
\1240\n\
\1542\n\
\1959\n\
\1022\n\
\1475\n\
\1733\n\
\1081\n\
\1889\n\
\1138\n\
\1757\n\
\1736\n\
\1723\n\
\1543\n\
\1820\n\
\1128\n\
\1039\n\
\1683\n\
\1477\n\
\1375\n\
\1499\n\
\676\n\
\1195\n\
\1250\n\
\220\n\
\1581\n\
\1328\n\
\1187\n\
\1485\n\
\1216\n\
\1769\n\
\1139\n\
\1064\n\
\1908\n\
\1516\n\
\1490\n\
\1419\n\
\1749\n\
\1347\n\
\1758\n\
\1024\n\
\1053\n\
\1842\n\
\1861\n\
\1403\n\
\1966\n\
\1546\n\
\1134\n\
\1593\n\
\1734\n\
\1916\n\
\1867\n\
\1101\n\
\1126\n\
\1301\n\
\1841\n\
\1515\n\
\1244\n\
\1401\n\
\1637\n\
\1054\n\
\1309\n\
\1933\n\
\1512\n\
\1263\n\
\1815\n\
\1634\n\
\1823\n\
\1295\n\
\1583\n\
\1104\n\
\1765\n\
\1850\n\
\1311\n\
\1692\n\
\1905\n\
\1149\n\
\1780\n\
\1330\n\
\1666\n\
\996\n\
\1913\n\
\1140\n\
\1089\n\
\1484\n\
\1356\n\
\1296\n\
\1323\n\
\1160\n\
\1881\n\
\1123\n\
\1166\n\
\1929"