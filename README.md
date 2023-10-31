# PFL - TP1 - Isaac

## **Authors**
- [João Macedo Lima](up202108891@fe.up.pt) - up202108891
- [Tomás Eiras Silva Martins](up202108776@fe.up.pt) - up202108776
  
## **Game description**
- Isaac is played on a 10x10 board, and it has 2 phases. There are two players: Dark and Light. Each player has 15 rectangular tiles of 5 different sizes. Each tile has a score written on it.

### The placement phase (begins with an empty board)
- Starting with the Dark, players take turns placing one of their tiles per turn onto unoccupied cells of the board.
- If a player cannot put any of his remaining tiles on his turn, then he passes, and the other player puts any tiles he can.
- It ends when both players don't have space to put any of their tiles on the board. All the remaining tiles are set apart and serve the role of the tiebreaker. If both players get the same score by the end of the scoring phase, each player forms a line with all his unplayed tiles and the player with the longest line of the unplayed tiles wins the game. If the lines are of equal length, the player who started the game wins. Note that the length of each tile is not a round number of board squares but a little bit shorter.

### The scoring phase
- At the beginning both players have 0 points. To indicate this both players put their score counter on the 00 square according to their own coordinates. If the mentioned square is under some tile, the score counter goes on top of this tile.
- The first player that passed in the Placement phase begins the Scoring phase. The players take turns removing one of their tiles from the board. The removed tile must be at least as long as any tile previously removed by the same player (e.g. if a player has previously removed a 4-cells tile then he can't remove his 3-cells tiles anymore). A player may not remove a tile lying under a score counter (of any colour).
- The result is the score earned by the removal of the tile. It indicates the maximum number of squares that the player's score counter may be moved.
- The counter can’t be placed in the same cell as the opponent's counter.
- The game ends when any player scores at least 100 points. If a player cannot remove any of his tiles on his turn, he passes and is out of the game. The game ends when both players have passed.

### End of the game 
- The game ends when a player scores at least 100 points. If a player cannot remove one of his/her pieces, he/she passes and is out of the game. It ends when both players have passed.

## **Game Logic**

### Internal representation of the game state

<p> This game has a 10x10 board and will be represented by a list of lists, where each list represents a row of the board. Each cell of the board is represented by a tuple of 2 elements: the first element is the colour of the tile that is on the cell, and the second element is the value of the tile that is on the cell. If there is no tile on the cell, there is no tuple and the cell will be with the following symbol ' - '. The second element (value) is connected to the value of the piece on the board as it will be described below. </p>
</p>

Possible values for the first element of the tuple:
- D (dark tile)
- L (light tile)

Possible values for the second element of the tuple and their respective length:
- 1 -> 3
- 2 -> 4
- 3 -> 5
- 4 -> 6
- 6 -> 7
  
Number of each type of tile for each player:
- 5 tiles of value 1
- 4 tiles of value 2
- 3 tiles of value 3
- 2 tiles of value 4
- 1 tile of value 6

<p>When the game board is displayed, empty cells are represented by a space with ' - '. Cells containing dark tiles are indicated by the letter 'D' followed by the tile's value, while cells with light tiles are represented by the letter 'L' followed by the tile's value. To determine whose turn it is, the current player's name will be prominently displayed at the beginning of each turn. Following the 'placement phase', players can view the remaining tiles that were not placed on the board. During the 'scoring phase', players have access to information about their counter's position on the board, and the current point totals.</p>


### Game state representation
- Placement Phase (empty board):


```pl

[[' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - '' - ',],
 [' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - '' - ',],
 [' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - '' - ',],
 [' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - '' - ',],
 [' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - '' - ',],
 [' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - '' - ',],
 [' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - '' - ',],
 [' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - '' - ',],
 [' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - '' - ',],
 [' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - '' - ',]]

 ```

 ```

 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 |    |  9  |  8  |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |     |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 90 |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |   0 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 80 |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  10 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 70 |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  20 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 60 |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  30 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 50 |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  40 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 40 |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  50 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 30 |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  60 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 20 |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  70 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 10 |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  80 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 0  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  90 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 |    |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |     |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 

Current player: Light

Choose a piece (value) [3-7] to move:
|- 

Choose a column [0-9] and a row [0-9] to move the piece to:
|- 

Remaining pieces for Light pieces player:
 -5 pieces of value 1 (size 3)
 -4 pieces of value 2 (size 4)
 -3 pieces of value 3 (size 5)
 -2 pieces of value 4 (size 6)
 -1 pieces of value 6 (size 7)

Remaining pieces for Dark pieces player:
 -5 pieces of value 1 (size 3)
 -4 pieces of value 2 (size 4)
 -3 pieces of value 3 (size 5)
 -2 pieces of value 4 (size 6)
 -1 pieces of value 6 (size 7)

Counter position for Light player: (0, 0)
Score for Light player: 0

Counter position for Dark player: (0, 0)
Score for Dark player: 0
```

- Placement Phase (board with tiles):

```pl

[[('D'-3), ('D'-3), ('D'-3), ('D'-3), ('D'-3),' - ',' - ', ('D'-2),' - ', ('L'-3)],
 [('L'-2), ('L'-4), ('L'-4), ('L'-4), ('L'-4), ('L'-4), ('L'-4), ('D'-2), ('D'-3), ('L'-3)],
 [('L'-2), ('D'-1), ('D'-3), ('D'-3), ('D'-3), ('D'-3), ('D'-3), ('D'-2), ('D'-3), ('L'-3)],
 [('L'-2), ('D'-1),' - ', ('D'-2), ('D'-2), ('D'-2), ('D'-2), ('D'-2), ('D'-3), ('L'-3)],
 [('L'-2), ('D'-1), ('L'-4), ('L'-4), ('L'-4), ('L'-4), ('L'-4), ('L'-4), ('D'-3), ('L'-3)],
 [('D'-1), ('D'-1), ('D'-1), ('L'-2), ('L'-2), ('L'-2), ('L'-2),' - ', ('D'-3), ' - ' ],
 [('D'-1), ('L'-1), ('L'-1), ('L'-1), ('L'-1), ('D'-3), ('D'-3), ('D'-3), ('D'-3), ('D'-3)],
 [('D'-1), ('L'-1),' - ', ('L'-1), ('L'-1), ('L'-1),' - ', ('L'-1), ('L'-1), ('D'-1)],
 [('D'-1), ('L'-1), ('D'-2), ('D'-2), ('D'-2), ('D'-2),' - ', ('L'-1), ('L'-1), ('D'-1)],
 [('D'-6), ('D'-6), ('D'-6), ('D'-6), ('D'-6), ('D'-6), ('D'-6), ('L'-1), ('L'-1), ('D'-1)]]
 ```

```
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 |    |  9  |  8  |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |     |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 90 | D-3 | D-3 | D-3 | D-3 | D-3 |  -  |  -  | D-2 |  -  | L-3 |   0 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 80 | L-2 | L-4 | L-4 | L-4 | L-4 | L-4 | L-4 | D-2 | D-3 | L-3 |  10 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 70 | L-2 | D-1 | D-3 | D-3 | D-3 | D-3 | D-3 | D-2 | D-3 | L-3 |  20 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 60 | L-2 | D-1 |  -  | D-2 | D-2 | D-2 | D-2 | D-2 | D-3 | L-3 |  30 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 50 | L-2 | D-1 | L-4 | L-4 | L-4 | L-4 | L-4 | L-4 | D-3 | L-3 |  40 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 40 | D-1 | D-1 | D-1 | L-2 | L-2 | L-2 | L-2 |  -  | D-3 |  -  |  50 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 30 | D-1 | L-1 | L-1 | L-1 | L-1 | D-3 | D-3 | D-3 | D-3 | D-3 |  60 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 20 | D-1 | L-1 |  -  | L-1 | L-1 | L-1 |  -  | L-1 | L-1 | D-1 |  70 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 10 | D-1 | L-1 | D-2 | D-2 | D-2 | D-2 |  -  | L-1 | L-1 | D-1 |  80 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 0  | D-6 | D-6 | D-6 | D-6 | D-6 | D-6 | D-6 | L-1 | L-1 | D-1 |  90 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 |    |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |     |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 

Current player: Light

Choose a piece (value) [3-7] to move:
|- 

Choose a column [0-9] and a row [0-9] to move the piece to:
|- 

Counter position for Light player: (0, 0)
Score for Light player: 0

Counter position for Dark player: (0, 0)
Score for Dark player: 0
```

- Scoring Phase (final):
  
```pl
[[('D'-3), ('D'-3), ('D'-3), ('D'-3), ('D'-3),' - ',' - ',' - ',' - ',' - '],
 [' - ', ('L'-4), ('L'-4), ('L'-4), ('L'-4), ('L'-4), ('L'-4),' - ', ('D'-3),' - '],
 [' - ', ' - ',' - ',' - ',' - ',' - ',' - ',' - ', ('D'-3), ('L'-3)],
 [('L'-2), ('D'-1),' - ',' - ',' - ',' - ',' - ',' - ', ('D'-3),' - '],
 [' - ', ' - ', ('L'-4), ('L'-4), ('L'-4), ('L'-4), ('L'-4), ('L'-4), ('D'-3),' - '],
 [' - ', ' - ',' - ',' - ',' - ',' - ',' - ',' - ', ('D'-3),' - '],
 [' - ', ' - ', ('L'-1), ('L'-1), ('L'-1), ('D'-3), ('D'-3), ('D'-3), ('D'-3), ('D'-3)],
 [' - ', ' - ',' - ',' - ',' - ',' - ',' - ',' - ',' - ', ('D'-1)],
 [' - ', ' - ', ('D'-2), ('D'-2), ('D'-2), ('D'-2),' - ',' - ',' - ', ('D'-1)],
 [('D'-6), ('D'-6), ('D'-6), ('D'-6), ('D'-6), ('D'-6), ('D'-6),' - ',' - ', ('D'-1)]]
``` 

```

 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 |    |  9  |  8  |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |     |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 90 | D-3 | D-3 | D-3 | D-3 | D-3 |  -  |  -  |  -  |  -  |  -  |   0 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 80 |  -  | L-4 | L-4 | L-4 | L-4 | L-4 | L-4 |  -  | D-3 |  -  |  10 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 70 |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  | D-3 | L-3 |  20 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 60 | L-2 | D-1 |  -  |  -  |  -  |  -  |  -  |  -  | D-3 |  -  |  30 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 50 |  -  |  -  | L-4 | L-4 | L-4 | L-4 | L-4 | L-4 | D-3 |  -  |  40 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 40 |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  | D-3 |  -  |  50 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 30 |  -  |  -  | L-1 | L-1 | L-1 | D-3 | D-3 | D-3 | D-3 | D-3 |  60 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 20 |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  |  -  | D-1 |  70 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 10 |  -  |  -  | D-2 | D-2 | D-2 | D-2 |  -  |  -  |  -  | D-1 |  80 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 | 0  | D-6 | D-6 | D-6 | D-6 | D-6 | D-6 | D-6 |  -  |  -  | D-1 |  90 |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 
 |    |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |     |
 |----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----| 

Current player: Dark

Counter position for Light player: (2, 4)
Score for Light player: 24

Counter position for Dark player: (2, 8)
Score for Dark player: 28

No more tiles to remove. Game ends. 
Player Dark wins!
```
**InitialBoard**