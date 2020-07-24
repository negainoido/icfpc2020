### Local Tournament AI
#### Attacker
#### Defender
Our defender AI imitates Unagi's elegant defender. 
It tries to survive as long as possible by increasing the number of ships.
We start with 100 units and increase the number of ships by repeating the following steps: 
1. Each ship with more than one units finds and follows the shortest list of commands to reach an unoccipied orbit. 
Here we consider an orbit unoccipied when there is not any ship orbiting on or planning to reach it. 
2. Once a ship with more than one units reaches an orbit where itself is an only ship orbiting on, it tries to clone and create a new ship.
   1. If it has large enough units (>= 14), a cloned ship gets a half of the status of its parent. 
   2. Otherwise, the status of a cloned ship is (0, 0, 0, 1). 

The step 2.i increases the number of ships exponentially. It allowed us to survive from top teams' beam attackers. 
