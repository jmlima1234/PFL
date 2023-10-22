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

<p> This game has a 10x10 board and will be represented by a list of lists, where each list represents a row of the board. Each cell of the board is represented by a tuple of 2 elements: the first element is the colour of the tile that is on the cell, and the second element is the value of the tile that is on the cell. If there is no tile on the cell, the tuple is (e, 0). The second element (value) is connected to the length of the piece on the board as it will be described below. </p>
</p>

Possible values for the first element of the tuple:
- e (empty cell)
- b (black tile)
- w (white tile)

Possible values for the second element of the tuple and their respective length:
- 0 - 0
- 1 - 3
- 2 - 4
- 3 - 5
- 4 - 6
- 6 - 7
  
Number of each type of tile for each player:
- 5 tiles of value 1
- 4 tiles of value 2
- 3 tiles of value 3
- 2 tiles of value 4
- 1 tile of value 6

<p>When the game board is displayed, empty cells are represented by a blank space. Cells containing black tiles are indicated by the letter 'B' followed by the tile's value, while cells with white tiles are represented by the letter 'W' followed by the tile's value. To determine whose turn it is, the current player's name will be prominently displayed at the beginning of each turn. Following the 'placement phase', players can view the remaining tiles that were not placed on the board. During the 'scoring phase', players have access to information about their counter's position on the board, and the current point totals.</p>

```pl
Game state representation

- Placement Phase (empty board):
[   [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0)],
    [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0)],
    [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0)],
    [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0)],
    [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0)],
    [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0)],
    [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0)],
    [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0)],
    [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0)],
    [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0)]]

current_player('white').
remaining_tiles('white',[(w,1), (w,1), (w,1), (w,1), (w,1), (w,2), (w,2), (w,2), (w,2), (w,3), (w,3), (w,3), (w,4), (w,4), (w,6)]).
score_counter('white', 0, 0).
score_counter('black', 9, 9).
total_score('white', 0).
total_score('black', 0).

- Placement Phase (board with tiles):
[   [(b, 3), (b, 3), (b, 3), (b, 3), (b, 3), (e, 0), (e, 0), (b, 2), (e, 0), (w, 3)],
    [(w, 2), (w, 4), (w, 4), (w, 4), (w, 4), (w, 4), (w, 4), (b, 2), (b, 3), (w, 3)],
    [(w, 2), (b, 1), (b, 3), (b, 3), (b, 3), (b, 3), (b, 3), (b, 2), (b, 3), (w, 3)],
    [(w, 2), (b, 1), (e, 0), (b, 2), (b, 2), (b, 2), (b, 2), (b, 2), (b, 3), (w, 3)],
    [(w, 2), (b, 1), (w, 4), (w, 4), (w, 4), (w, 4), (w, 4), (w, 4), (b, 3), (w, 3)],
    [(b, 1), (b, 1), (b, 1), (w, 2), (w, 2), (w, 2), (w, 2), (e, 0), (b, 3), (e, 0)],
    [(b, 1), (w, 1), (w, 1), (w, 1), (w, 1), (b, 3), (b, 3), (b, 3), (b, 3), (b, 3)],
    [(b, 1), (w, 1), (e, 0), (w, 1), (w, 1), (w, 1), (e, 0), (w, 1), (w, 1), (b, 1)],
    [(b, 1), (w, 1), (b, 2), (b, 2), (b, 2), (b, 2), (e, 0), (w, 1), (w, 1), (b, 1)],
    [(b, 6), (b, 6), (b, 6), (b, 6), (b, 6), (b, 6), (b, 6), (w, 1), (w, 1), (b, 1)]]

current_player('white').
remaining_tiles('white',[(w,2), (w,2), (w,3), (w,3), (w,6)]).
score_counter('white', 0, 0).
score_counter('black', 9, 9).
total_score('white', 0).
total_score('black', 0).

- Scoring Phase (final):
[   [(b, 3), (b, 3), (b, 3), (b, 3), (b, 3), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0)],
    [(e, 0), (w, 4), (w, 4), (w, 4), (w, 4), (w, 4), (w, 4), (e, 0), (b, 3), (e, 0)],
    [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (b, 3), (w, 3)],
    [(w, 2), (b, 1), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (b, 3), (e, 0)],
    [(e, 0), (e, 0), (w, 4), (w, 4), (w, 4), (w, 4), (w, 4), (w, 4), (b, 3), (e, 0)],
    [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (b, 3), (e, 0)],
    [(e, 0), (e, 0), (w, 1), (w, 1), (w, 1), (b, 3), (b, 3), (b, 3), (b, 3), (b, 3)],
    [(e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (e, 0), (b, 1)],
    [(e, 0), (e, 0), (b, 2), (b, 2), (b, 2), (b, 2), (e, 0), (e, 0), (e, 0), (b, 1)],
    [(b, 6), (b, 6), (b, 6), (b, 6), (b, 6), (b, 6), (b, 6), (e, 0), (e, 0), (b, 1)]]

current_player('black').
remaining_tiles('black',[(b,1), (b,2), (b,4), (b,4)]).
score_counter('white', 4, 2).
score_counter('black', 2, 7).
total_score('white', 24).
total_score('black', 28).

No more tiles to remove. Game ends.
Player 'black' wins!
```