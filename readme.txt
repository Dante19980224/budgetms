TO start the game, in terminal:
	make utop 
In utop, there are 2 ways:
	main ();;  (default level 1)
	select_level n;;  (replace n with a number from 1 to 3, currently only 3 levels available)


Controls:
"wsad" to move the cursor
"m" to mark/unmark a block that is not yet revealed
"n" to reveal a block that is not yet revealed
"q" to quit


About minesweeper:
-win when all empty blocks(not mines) are revealed
 and all mines are not revealed
-lose when revealing a mine block
-mine block locations are random
-to play the same level with different mine locations, restart utop


level details:
level 1 - 5x5, 5 mines
level 2 - 10x10, 10 mines
level 3 - 10x10, 20 mines



potential upgrades:
-cannot reveal marked block
-clicking on a number reveals adjacent unmarked blocks, IFF
	 the number of adjacent unmarked blocks equals the number
-add timer. save and display fastest record