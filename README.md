# TileClick

TileClick is a small 2D tile-click RPG written in F# with Raylib-cs. The player moves by clicking tiles, explores multiple maps, fights random monsters, equips items, saves/loads progress, and wins by defeating the final boss in `map3.txt`.


## Requirements

- .NET 10 SDK
- A desktop environment that can open a Raylib window
- Tested on Windows 10/11

This project uses Raylib-cs. Raylib is cross-platform, so the project may also run on Linux or macOS with .NET 10 and the required graphics/native library support, but the project was tested on Windows.

## How to run

inside your folder for the game,
```bash
git clone https://github.com/seunghyeon-bang/TileClick.git
cd TileClick
dotnet restore
dotnet run
```

The repository should contain these files in the project folder:

- `Program.fs`
- `TileClick.fsproj`
- `tileset32.png`
- `map.txt`
- `map2.txt`
- `map3.txt`

## Controls

- Left mouse click: move to a reachable tile
- `I`: open/close inventory
- Click an item in the bag: equip it
- Click equipped weapon/armor slot: unequip it
- `A`: attack during battle
- `R`: run during normal battles
- `E`: sleep when standing on the bed tile
- `F5`: save
- `F9`: load
- `ESC`: close the game

## Game flow
- '6','7' refers to cave entrances.
- player level up by hunting monsters in map2.
1. The player starts in `map.txt`. This is the homeground and no monsters spawn.
2. Stepping on portal tile `6`(cave entrance in the game) in `map.txt` moves the player to `map2.txt`.
3. In `map2.txt`, stepping on tile `7` returns to `map.txt`, and stepping on tile `6` moves to `map3.txt`.
4. In `map3.txt`, stepping on tile `7` returns to `map2.txt`.
5. In `map3.txt`, tile `9` is the boss tile. When the player steps on it, the final boss appears.
6. The boss cannot be escaped from. Defeating the boss shows the victory screen and saves progress automatically.

## Save file

The game writes progress to `save.json`. This file is generated while playing and does not need to be committed.

## LLM usage
My usage for LLM is mainly because I didn't know very well about the Raylib library functions.
I used ChatGPT to help revise the F# code, especially for connecting `map2.txt` to `map3.txt`, adding boss tile behavior for tile `9`, adding a victory state, and updating save/load behavior. I manually checked the requested behavior against my map files and project requirements. 
The main thing the LLM could not verify directly was whether the project builds on my local Windows/.NET/Raylib environment, so I still need to run `dotnet run` locally before final submission.
Also I had to reprompt to the LLM because the place of the text showing state of the battle was not appropriate.
The main point that LLM was not able to do correctly was creating the game interface like showing text and inventories more human readable and friendly way. So I had to fix the code several times to make it better.
