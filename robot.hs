robot (name, attack, hp) = \message -> message (name, attack, hp)

name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHp aRobot = aRobot hp

setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHp aRobot newHp = aRobot (\(n,a,h) -> robot (n,a,newHp))

printRobot aRobot = aRobot(\(n,a,h) -> n ++
                                       " attack:" ++ (show a) ++
                                       " hp:" ++ (show h))

damage aRobot attackDamage = aRobot (\(n,a,h) ->
                                        robot (n,a,h-attackDamage))

fight aRobot defender = damage defender attack
    where attack = if getHp aRobot > 10
                   then getAttack aRobot
                   else 0

mapRobots robotList = map (\aRobot -> getHp aRobot) robotList

robotsList = [robot ("speedy", 15, 402), 
              robot ("speedyy", 15, 401), 
              robot ("speedyy", 15, 403)]
robot4 = robot ("yy", 15, 403)

fightRobot4 = fight robot4

