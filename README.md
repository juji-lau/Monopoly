# CS-3110-Final-Project

## Overview:
The project is a command-line and GUI-based Monopoly game implemented in OCaml.
The game a basic edition of the multiplayer game, Monopoly.  It launches an interactive window, allowing each player to throw a die, move spaces, buy property, pay and collect rent, go to and get out of jail, or quit the game on their turn. Players can view the Monopoly board, their movement, bank accounts, properties owned, and general state of the game.
Players interact with the game through a combination of command-line input (CLI) and a GUI window powered by `Raylib`.

**Available Player Actions:**
- _Roll dice and move_: The player.ml module has a move function.
- _Buy property_: If a player lands on an unowned property, they can choose to buy it. This is handled by player.ml's buy_property function, which deducts the cost from their account.
- _Pay/Collect Rent_: Players automatically pay rent based if they land on an owned square.
- _Go to/Get out of Jail_: player.ml includes go_to_jail and free_from_jail functions.
- _Manage Finances_: Players' accounts are updated for purchases, rent, taxes, and other card effects.
- _Handle Chance/Community Chest_: The position.ml file lists text for Chance and Community Chest cards, some of which result in direct payments or movement.
- _End Turn/Quit Game_: Players can exit or quit the game upon "end" and "quit".


**Prerequisite:** The game's GUI is intended to work on a Mac computer.

### How to Run:
(1) Clone the code: 
    `cd <target directory>`
    `git clone https://github.coecis.cornell.edu/sz266/CS-3110-Final-Project/tree/working_json/src`

(2) Install Dependencies:
    `opam install ANSITerminal`
    `opam install Yojson`
    `opam install Raylib`

(3) Clean and build:
    `make clean`
    `make build`

(4) Play the game:
    `make clean`
    `make play`


#### File Breakdown:
- `monopoly/data/squares.json` stores the game board including properties, railroads, utilities, special squares (Go, Jail, Chance, Community Chest, Taxes, Free Parking), their names, costs, rents, and other attributes.
- `position.ml` parses the JSON data, retrieving square information (name, index, cost), and loading the board layout from `squares.json`.
- `player.ml` defines the player's state, including their name, account balance, current position on the board, owned properties, color (for GUI), and jail status. It allows players to move, buy property, check owned properties (including counts for rails/utilities), and manage their account (deposit/withdraw).
- `account.ml` handles player account balances, allowing them to get the current balance, make payments (if they have the funds), and receive money.
- `command.ml` parses text-based commands entered by the player, such as "roll", "purchase", "end", and "quit", in a way that the game engine can process.

### Contributors:
Sophie (sz266)
Alexandra (aom52)
Juji (jal499)
Brooke Hudson (bah253)

**Note:** This is my (Juji's) version of the final project for CS3110, which I'm saving in my personal repo to keep as my school GitHub enterprise account gets deleted.
