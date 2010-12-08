Clans
===== 

Computer version of the board game [CLANS](http://en.wikipedia.org/wiki/Clans_(board_game))

Gameplay
--------

At the start of the game, each player (from 2 to 5) is secretly assigned one of five
colors - red, yellow, blue, green, or white.
Although each player knows their own color, they do not know the color of the other players.
The game begins with one hut on each space, the colors being randomly distributed across the huts.
On their turn, a player moves all of the huts from one space to another space.
When a group of huts is completely isolated (all the surrounding spaces are empty), 
that space is scored.
Scoring is different based on the terrain that those huts are on.
The person with the most huts on that space, and anyone tied for the most, gets points.
The game ends when the moves have been exhausted.

How to play
-----------

On the title screen, select the number of player with LEFT and RIGHT arrows
then press SPACE to play. Be sure to choose a player number before you start.

On the game screen, players scores are displayed in the top-right corner of the screen,
current player number in the bottom left one.
Move huts by clicking on the territory to start from then on the destination territory.
Press SPACE to display current game informations: current player's color, bonus and malus terrains.
Be sure that only the current player is watching when you do this so player's color isn't revealed to
the others.

Moving huts
-----------

Huts can only move from one territory to an adjacent one.
They can only move to a non-empty territory.
You can't move huts from a territory if there's more than 7 huts on it.

Village creation
----------------

A village is created on a territory when all adjacent territories are empty.
Multiple villages may be created at the same time, the creator of those villages
may choose in which order they're created.
At most 12 villages may be created.
Additional villages created after the last one are just ignored.


Score
-----

Score comes from creating villages.
When this happens, the following rules apply:

- if the territory where the village is created is of the current special destructive
type, the village is destroyed and score is zero.
- if the territory where the village is created is of the current special bonus
type, bonus points associated with this type are added to the score.
- if there's at least one hut of each color and at least two huts of one of these colors on
a territory, all singles of a color are ignored.
(So if you have one hut for four of the colors and 3 of the last color, only those 3 are kept for
score computation);


Then score is calculated by counting one point per hut that remains in the village
after removing singles.
This score is then added to each color that contributed to it (colors of the remaining huts).

One extra point is granted to the village creator and will be added to its score at the end
of the game when player colors are revealed.

Game end
--------

All extra points are added the their respective players' scores.
The player with the highest score wins.
