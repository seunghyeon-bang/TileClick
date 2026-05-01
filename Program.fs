open System
open System.IO
open System.Numerics
open System.Collections.Generic
open System.Text.Json
open Raylib_cs

// Raylib-cs uses CBool in many calls
let inline toBool (x: CBool) : bool = CBool.op_Implicit x

type Pos = { x:int; y:int }

// ------------------ Inventory / Equipment ------------------

type Item =
    | Weapon of name:string * atk:int
    | Armor  of name:string * def:int

let itemName =
    function
    | Weapon (n, _) -> n
    | Armor  (n, _) -> n

let itemDesc =
    function
    | Weapon (_, atk) -> $"Weapon (+{atk} ATK)"
    | Armor  (_, def) -> $"Armor (+{def} DEF)"

// A tiny item "database" so we can reconstruct from saved names
let allItems : Item list =
    [ Weapon("Wood Sword", 2)
      Weapon("Iron Sword", 4)
      Armor ("Cloth Armor", 1)
      Armor ("Leather Armor", 2) ]

let tryFindItemByName (name:string) =
    allItems |> List.tryFind (fun it -> itemName it = name)

type Equipment =
    { Weapon : Item option
      Armor  : Item option }

type Character =
    { Level   : int
      XP      : int
      NextXP  : int
      HP      : int
      MaxHP   : int
      BaseATK : int
      BaseDEF : int
      Equip   : Equipment
      Bag     : Item list }

let equipBonus (equip: Equipment) =
    let atk =
        match equip.Weapon with
        | Some (Weapon(_, a)) -> a
        | _ -> 0
    let def =
        match equip.Armor with
        | Some (Armor(_, d)) -> d
        | _ -> 0
    atk, def

let effectiveATK (c:Character) =
    let atkB, _ = equipBonus c.Equip
    c.BaseATK + atkB

let effectiveDEF (c:Character) =
    let _, defB = equipBonus c.Equip
    c.BaseDEF + defB

let addToBag (it:Item) (bag:Item list) = it :: bag

let removeOneFromBag (it:Item) (bag:Item list) =
    let mutable removed = false
    bag
    |> List.filter (fun x ->
        if not removed && x = it then
            removed <- true
            false
        else true)

let unequipWeapon (c:Character) =
    match c.Equip.Weapon with
    | None -> c
    | Some it ->
        { c with
            Equip = { c.Equip with Weapon = None }
            Bag   = addToBag it c.Bag }

let unequipArmor (c:Character) =
    match c.Equip.Armor with
    | None -> c
    | Some it ->
        { c with
            Equip = { c.Equip with Armor = None }
            Bag   = addToBag it c.Bag }

let equipItem (it:Item) (c:Character) =
    match it with
    | Weapon _ ->
        let c =
            match c.Equip.Weapon with
            | Some old -> { c with Bag = addToBag old c.Bag }
            | None -> c
        { c with
            Equip = { c.Equip with Weapon = Some it }
            Bag   = removeOneFromBag it c.Bag }
    | Armor _ ->
        let c =
            match c.Equip.Armor with
            | Some old -> { c with Bag = addToBag old c.Bag }
            | None -> c
        { c with
            Equip = { c.Equip with Armor = Some it }
            Bag   = removeOneFromBag it c.Bag }

let tryLevelUp (c:Character) =
    // HP stays the same (no auto heal)
    let mutable hero = c
    let mutable leveled = false
    while hero.XP >= hero.NextXP do
        leveled <- true
        let newLevel = hero.Level + 1
        let leftover = hero.XP - hero.NextXP
        let next = int (float hero.NextXP * 1.4) + 5
        let newMaxHP = hero.MaxHP + 3
        hero <-
            { hero with
                Level = newLevel
                XP = leftover
                NextXP = next
                MaxHP = newMaxHP
                HP = min hero.HP newMaxHP
                BaseATK = hero.BaseATK + 1
                BaseDEF = hero.BaseDEF + 1 }
    hero, leveled

// ------------------ Monsters / Battle ------------------

type MonsterKind = Weak | Medium

type MonsterTemplate =
    { Name : string
      MaxHP: int
      ATK  : int
      DEF  : int
      XP   : int
      Kind : MonsterKind }

type Monster =
    { Name : string
      MaxHP: int
      HP   : int
      ATK  : int
      DEF  : int
      XP   : int
      Kind : MonsterKind }

let makeMonster (t:MonsterTemplate) =
    { Name=t.Name; MaxHP=t.MaxHP; HP=t.MaxHP; ATK=t.ATK; DEF=t.DEF; XP=t.XP; Kind=t.Kind }

type BattleState =
    { Monster : Monster
      Log     : string list   // OLDEST-first
      Ended   : bool }

type GameMode =
    | Explore
    | Battle of BattleState

let addLog (s:string) (b:BattleState) =
    // append to end, keep last 50
    let newLog =
        (b.Log @ [s])
        |> (fun xs -> if xs.Length > 50 then xs |> List.skip (xs.Length - 50) else xs)
    { b with Log = newLog }

let rollDamage (rng:Random) (atk:int) (def:int) =
    let raw = atk - def + rng.Next(0, 3)
    max 1 raw

// ------------------ Tile / Map ------------------

let tileSize = 32
let scale = 4
let drawTileSize = tileSize * scale
let tileSizeF = float32 tileSize
let drawTileSizeF = float32 drawTileSize

// tileset indices (row 0)
let TILE_GRASS    = 0
let TILE_WATER    = 1
let TILE_WALL     = 2
let TILE_SAND     = 3
let TILE_TREE     = 4
let TILE_PLAYER   = 5
let TILE_MOUNTAIN = 6
let TILE_CAVE     = 7

// Map codes:
// 0 grass, 1 sand, 2 water(block), 3 wall(block), 4 tree(block), 5 mountain(block),
// 6 entrance, 7 exit, 8 bed
let mapCodeToTilesetIndex (code:int) =
    match code with
    | 0 -> TILE_GRASS
    | 1 -> TILE_SAND
    | 2 -> TILE_WATER
    | 3 -> TILE_WALL
    | 4 -> TILE_TREE
    | 5 -> TILE_MOUNTAIN
    | 6 -> TILE_CAVE
    | 7 -> TILE_CAVE
    | 8 -> TILE_GRASS
    | _ -> TILE_GRASS

let isWalkableCode (code:int) =
    code = 0 || code = 1 || code = 6 || code = 7 || code = 8

let loadMap (path:string) : int[,] =
    if not (File.Exists path) then
        failwith $"Can't find {path}. Put it in the project folder and set CopyToOutputDirectory."

    let lines =
        File.ReadAllLines(path)
        |> Array.map (fun s -> s.Trim())
        |> Array.filter (fun s -> s <> "")

    if lines.Length = 0 then failwith $"{path} is empty."

    let parseLine (s:string) =
        if s.IndexOfAny([|' '; '\t'|]) >= 0 then
            s.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
        else
            s.ToCharArray()
            |> Array.map (fun ch ->
                if Char.IsDigit ch then int ch - int '0'
                else failwith $"Invalid char '{ch}' in {path}. Use digits 0..9 only.")

    let rows = lines |> Array.map parseLine
    let w = rows[0].Length
    if w = 0 then failwith $"{path}: first row has no columns."
    if rows |> Array.exists (fun r -> r.Length <> w) then
        failwith $"{path}: all rows must have the same number of columns."

    let h = rows.Length
    Array2D.init h w (fun y x -> rows[y][x])

let tileToWorld (p:Pos) =
    Vector2(float32 p.x * drawTileSizeF, float32 p.y * drawTileSizeF)

let worldToTile (v:Vector2) =
    { x = int (v.X / drawTileSizeF); y = int (v.Y / drawTileSizeF) }

let clamp (v:float32) (lo:float32) (hi:float32) =
    max lo (min hi v)

let findFirstCode (mapCodes:int[,]) (code:int) : Pos option =
    let h = mapCodes.GetLength(0)
    let w = mapCodes.GetLength(1)
    let mutable found : Pos option = None
    let mutable y = 0
    while y < h && found.IsNone do
        let mutable x = 0
        while x < w && found.IsNone do
            if mapCodes[y, x] = code then found <- Some { x = x; y = y }
            x <- x + 1
        y <- y + 1
    found

let bfs (mapCodes:int[,]) (start:Pos) (goal:Pos) : Pos list option =
    let h = mapCodes.GetLength(0)
    let w = mapCodes.GetLength(1)

    let inBounds (p:Pos) = p.x >= 0 && p.y >= 0 && p.x < w && p.y < h
    let walkable (p:Pos) = inBounds p && isWalkableCode mapCodes[p.y, p.x]

    if start = goal then Some [start]
    elif not (walkable goal) then None
    else
        let q = Queue<Pos>()
        let cameFrom = Dictionary<Pos, Pos>() // child -> parent
        q.Enqueue(start)

        let neighbors (p:Pos) =
            [ {x=p.x+1;y=p.y}; {x=p.x-1;y=p.y}; {x=p.x;y=p.y+1}; {x=p.x;y=p.y-1} ]
            |> List.filter walkable

        let mutable found = false
        while q.Count > 0 && not found do
            let cur = q.Dequeue()
            for n in neighbors cur do
                if not (cameFrom.ContainsKey(n)) && n <> start then
                    cameFrom[n] <- cur
                    if n = goal then found <- true
                    q.Enqueue(n)

        if not found then None
        else
            let mutable path = [goal]
            let mutable cur = goal
            while cur <> start do
                cur <- cameFrom[cur]
                path <- cur :: path
            Some path

// ------------------ Saving / Loading ------------------

type SaveData =
    { CurrentMap : string
      PlayerX    : int
      PlayerY    : int
      Level      : int
      XP         : int
      NextXP     : int
      HP         : int
      MaxHP      : int
      BaseATK    : int
      BaseDEF    : int
      EquippedWeapon : string option
      EquippedArmor  : string option
      Bag        : string list }

let savePath = "save.json"

let saveGame (currentMap:string) (playerTile:Pos) (hero:Character) =
    let data : SaveData =
        { CurrentMap = currentMap
          PlayerX = playerTile.x
          PlayerY = playerTile.y
          Level = hero.Level
          XP = hero.XP
          NextXP = hero.NextXP
          HP = hero.HP
          MaxHP = hero.MaxHP
          BaseATK = hero.BaseATK
          BaseDEF = hero.BaseDEF
          EquippedWeapon = hero.Equip.Weapon |> Option.map itemName
          EquippedArmor  = hero.Equip.Armor  |> Option.map itemName
          Bag = hero.Bag |> List.map itemName }

    let opts = JsonSerializerOptions(WriteIndented = true)
    let json = JsonSerializer.Serialize(data, opts)
    File.WriteAllText(savePath, json)

let tryLoadGame () : SaveData option =
    if not (File.Exists savePath) then None
    else
        try
            let json = File.ReadAllText(savePath)
            let data = JsonSerializer.Deserialize<SaveData>(json)
            if isNull (box data) then None else Some data
        with _ ->
            None

let applySave (data:SaveData) =
    // reconstruct hero items from name DB
    let weapon =
        data.EquippedWeapon
        |> Option.bind tryFindItemByName
    let armor =
        data.EquippedArmor
        |> Option.bind tryFindItemByName
    let bag =
        data.Bag
        |> List.choose tryFindItemByName

    let hero =
        { Level = data.Level
          XP = data.XP
          NextXP = data.NextXP
          HP = data.HP
          MaxHP = data.MaxHP
          BaseATK = data.BaseATK
          BaseDEF = data.BaseDEF
          Equip = { Weapon = weapon; Armor = armor }
          Bag = bag }

    let pos = { x = data.PlayerX; y = data.PlayerY }
    data.CurrentMap, pos, hero

// ------------------ Main ------------------

[<EntryPoint; STAThread>]
let main _ =
    let rng = Random()

    // viewport (screen)
    let viewTilesW = 10
    let viewTilesH = 8
    let winW = viewTilesW * drawTileSize
    let winH = viewTilesH * drawTileSize

    Raylib.InitWindow(winW, winH, "TileClick")
    Raylib.SetTargetFPS(60)

    let tex = Raylib.LoadTexture("tileset32.png")
    Raylib.SetTextureFilter(tex, TextureFilter.Point)

    // Monsters
    let weakMonsters =
        [| { Name="Slime"; MaxHP=10; ATK=3; DEF=0; XP=4; Kind=Weak }
           { Name="Snake"; MaxHP=12; ATK=4; DEF=1; XP=5; Kind=Weak }
           { Name="Rat";   MaxHP= 8; ATK=3; DEF=0; XP=3; Kind=Weak } |]

    let mediumMonsters =
        [| { Name="Thief"; MaxHP=18; ATK=6; DEF=2; XP=10; Kind=Medium }
           { Name="Ox";    MaxHP=22; ATK=7; DEF=2; XP=12; Kind=Medium }
           { Name="Witch"; MaxHP=16; ATK=8; DEF=1; XP=12; Kind=Medium } |]

    let chooseMonster () =
        if rng.NextDouble() < 0.70 then makeMonster weakMonsters[rng.Next weakMonsters.Length]
        else makeMonster mediumMonsters[rng.Next mediumMonsters.Length]

    // Map state
    let mutable currentMap = "map.txt"
    let mutable mapCodes = loadMap currentMap
    let mutable mapH = mapCodes.GetLength(0)
    let mutable mapW = mapCodes.GetLength(1)
    let mutable worldW = float32 (mapW * drawTileSize)
    let mutable worldH = float32 (mapH * drawTileSize)

    let recomputeSizes () =
        mapH <- mapCodes.GetLength(0)
        mapW <- mapCodes.GetLength(1)
        worldW <- float32 (mapW * drawTileSize)
        worldH <- float32 (mapH * drawTileSize)

    let inBounds (p:Pos) = p.x >= 0 && p.y >= 0 && p.x < mapW && p.y < mapH
    let isWalkable (p:Pos) = inBounds p && isWalkableCode mapCodes[p.y, p.x]

    // Player movement
    let mutable playerTile = { x = 1; y = 1 }
    let mutable playerPos  = tileToWorld playerTile
    let mutable path : Pos list = []

    let tilesPerSecond = 5.0f
    let speed = tilesPerSecond * drawTileSizeF

    let mutable portalCooldown = 0.0f

    // Inventory
    let mutable inventoryOpen = false

    let mutable hero : Character =
        { Level   = 1
          XP      = 0
          NextXP  = 10
          HP      = 20
          MaxHP   = 20
          BaseATK = 3
          BaseDEF = 1
          Equip   = { Weapon = None; Armor = None }
          Bag     =
            [ Weapon("Wood Sword", 2)
              Armor ("Cloth Armor", 1)
              Weapon("Iron Sword", 4)
              Armor ("Leather Armor", 2) ] }

    // Toast
    let mutable toastText = ""
    let mutable toastTimer = 0.0f
    let toast (msg:string) =
        toastText <- msg
        toastTimer <- 2.2f

    // Game mode
    let mutable mode : GameMode = Explore

    // Camera
    let mutable cam : Camera2D = Camera2D()
    cam.Offset <- Vector2(float32 winW / 2.0f, float32 winH / 2.0f)
    cam.Rotation <- 0.0f
    cam.Zoom <- 1.0f
    cam.Target <- Vector2(0.0f, 0.0f)

    let switchMap (toMap:string) (spawnCode:int) =
        currentMap <- toMap
        mapCodes <- loadMap currentMap
        recomputeSizes()
        playerTile <-
            match findFirstCode mapCodes spawnCode with
            | Some p -> p
            | None -> { x = 1; y = 1 }
        playerPos <- tileToWorld playerTile
        path <- []
        portalCooldown <- 0.25f

    let respawnAtBed () =
        let setPos (p:Pos) =
            playerTile <- p
            playerPos <- tileToWorld p
            path <- []

        match findFirstCode mapCodes 8 with
        | Some bed ->
            setPos bed
        | None ->
            if File.Exists("map2.txt") then
                let map2 = loadMap "map2.txt"
                match findFirstCode map2 8 with
                | Some bed ->
                    currentMap <- "map2.txt"
                    mapCodes <- map2
                    recomputeSizes()
                    setPos bed
                | None ->
                    setPos { x = 1; y = 1 }
            else
                setPos { x = 1; y = 1 }

        hero <- { hero with HP = hero.MaxHP }

    let startBattle (m:Monster) =
        path <- []
        inventoryOpen <- false
        let b =
            { Monster = m
              Log = [ $"A wild {m.Name} appears!" ]
              Ended = false }
        mode <- Battle b

    // Encounters in map2
    let encounterChance = 0.12

    // --------- Load saved game if exists ----------
    // Start a new game by default.
    // Saved progress is loaded only when the user presses F9.

    // helper: autosave
    let autosave () =
        try
            saveGame currentMap playerTile hero
        with _ -> ()

    while not (Raylib.WindowShouldClose() |> toBool) do
        let dt = Raylib.GetFrameTime()
        if toastTimer > 0.0f then toastTimer <- max 0.0f (toastTimer - dt)

        // Manual save/load
        if Raylib.IsKeyPressed(KeyboardKey.F5) |> toBool then
            saveGame currentMap playerTile hero
            toast "Saved!"
        if Raylib.IsKeyPressed(KeyboardKey.F9) |> toBool then
            match tryLoadGame() with
            | Some data ->
                let (m, pos, h) = applySave data
                if File.Exists(m) then
                    currentMap <- m
                    mapCodes <- loadMap currentMap
                    recomputeSizes()
                hero <- h
                let pos =
                    { x = max 0 (min (mapW-1) pos.x)
                      y = max 0 (min (mapH-1) pos.y) }
                playerTile <- pos
                playerPos <- tileToWorld playerTile
                path <- []
                mode <- Explore
                inventoryOpen <- false
                toast "Loaded!"
            | None ->
                toast "No save.json to load."

        // Toggle inventory
        if mode = Explore && (Raylib.IsKeyPressed(KeyboardKey.I) |> toBool) then
            inventoryOpen <- not inventoryOpen

        if portalCooldown > 0.0f then
            portalCooldown <- max 0.0f (portalCooldown - dt)

        let oldTile = playerTile

        // ------------------ UPDATE ------------------

        match mode with
        | Explore ->
            // Sleep on bed (press E anytime you're standing on it)
            if inBounds playerTile && mapCodes[playerTile.y, playerTile.x] = 8 then
                if Raylib.IsKeyPressed(KeyboardKey.E) |> toBool then
                    hero <- { hero with HP = hero.MaxHP }
                    toast "You slept. HP fully restored!"
                    autosave()

            // Click-to-move disabled while inventory open
            if (not inventoryOpen) && (Raylib.IsMouseButtonPressed(MouseButton.Left) |> toBool) then
                let mouseWorld = Raylib.GetScreenToWorld2D(Raylib.GetMousePosition(), cam)
                let targetTile = worldToTile mouseWorld
                if isWalkable targetTile then
                    match bfs mapCodes playerTile targetTile with
                    | Some p -> path <- (p |> List.tail)
                    | None -> path <- []

            // Inventory clicks (screen space)
            if inventoryOpen && (Raylib.IsMouseButtonPressed(MouseButton.Left) |> toBool) then
                let m = Raylib.GetMousePosition()
                let mx, my = int m.X, int m.Y

                let panelW, panelH = 360, 440
                let panelX, panelY = winW - panelW - 20, 20

                let inside x y w h =
                    mx >= x && mx < x+w && my >= y && my < y+h

                let slotW, slotH = panelW - 40, 44
                let weaponRect = (panelX+20, panelY+140, slotW, slotH)
                let armorRect  = (panelX+20, panelY+200, slotW, slotH)

                let (wx,wy,ww,wh) = weaponRect
                let (ax,ay,aw,ah) = armorRect

                if inside wx wy ww wh then
                    hero <- unequipWeapon hero
                    autosave()
                elif inside ax ay aw ah then
                    hero <- unequipArmor hero
                    autosave()
                else
                    let listX = panelX + 20
                    let listY = panelY + 290
                    let rowH  = 28
                    let listW = panelW - 40
                    let listH = panelH - (listY - panelY) - 20

                    if inside listX listY listW listH then
                        let idx = (my - listY) / rowH
                        let bagArr = hero.Bag |> List.toArray
                        if idx >= 0 && idx < bagArr.Length then
                            hero <- equipItem bagArr[idx] hero
                            autosave()

            // Smooth movement along path
            match path with
            | next :: rest ->
                let targetPos = tileToWorld next
                let delta = targetPos - playerPos
                let dist = delta.Length()

                if dist < 0.5f then
                    playerPos <- targetPos
                    playerTile <- next
                    path <- rest
                else
                    let step = speed * dt
                    if dist <= step then
                        playerPos <- targetPos
                        playerTile <- next
                        path <- rest
                    else
                        let dir = Vector2.Normalize(delta)
                        playerPos <- playerPos + dir * step
            | [] -> ()

            // Tile-arrival events
            if playerTile <> oldTile && inBounds playerTile then
                let code = mapCodes[playerTile.y, playerTile.x]

                // Portals
                if portalCooldown <= 0.0f then
                    match currentMap, code with
                    | "map.txt", 6 ->
                        switchMap "map2.txt" 7
                        autosave()
                    | "map2.txt", 7 ->
                        switchMap "map.txt" 6
                        autosave()
                    | _ -> ()

                // Random encounters
                let canEncounter =
                    currentMap = "map2.txt" &&
                    (not inventoryOpen) &&
                    (code = 0 || code = 1)
                if canEncounter && rng.NextDouble() < encounterChance then
                    startBattle (makeMonster (if rng.NextDouble() < 0.70 then weakMonsters[rng.Next weakMonsters.Length] else mediumMonsters[rng.Next mediumMonsters.Length]))

        | Battle b ->
            let mutable battle = b

            if battle.Ended then
                if Raylib.IsKeyPressed(KeyboardKey.Space) |> toBool then
                    respawnAtBed()
                    mode <- Explore
                    toast "You woke up at the bed. HP restored!"
                    autosave()
            else
                if hero.HP <= 0 then
                    battle <- battle |> addLog "You were defeated... (press SPACE to respawn)"
                    battle <- { battle with Ended = true }
                else
                    let doMonsterAttack () =
                        let dmg = rollDamage rng battle.Monster.ATK (effectiveDEF hero)
                        hero <- { hero with HP = max 0 (hero.HP - dmg) }
                        battle <- battle |> addLog ($"{battle.Monster.Name} hits you for {dmg} damage!")
                        if hero.HP <= 0 then
                            battle <- battle |> addLog "You were defeated... (press SPACE to respawn)"
                            battle <- { battle with Ended = true }

                    let winBattle () =
                        let gained = battle.Monster.XP
                        hero <- { hero with XP = hero.XP + gained }
                        let oldLevel = hero.Level
                        let (hero2, leveled) = tryLevelUp hero
                        hero <- hero2

                        // ✅ Better toast: battle won + XP gained and current XP status
                        toast $"Battle won! +{gained} XP (XP: {hero.XP}/{hero.NextXP})"
                        // ✅ If leveled, notify
                        if leveled then
                            toast $"Level up! You are now Lv {hero.Level} (HP stays {hero.HP}/{hero.MaxHP})"

                        // Also add to log (optional)
                        battle <- battle |> addLog ($"{battle.Monster.Name} is defeated! You gain {gained} XP.")
                        if leveled then battle <- battle |> addLog ($"Level up! You are now Lv {hero.Level}.")

                        mode <- Explore
                        autosave()

                    let doPlayerAttack () =
                        let dmg = rollDamage rng (effectiveATK hero) battle.Monster.DEF
                        let newHP = max 0 (battle.Monster.HP - dmg)
                        battle <- { battle with Monster = { battle.Monster with HP = newHP } }
                        battle <- battle |> addLog ($"You deal {dmg} damage to {battle.Monster.Name}!")
                        if newHP <= 0 then
                            winBattle()
                        else
                            doMonsterAttack()

                    let tryRun () =
                        let baseChance =
                            match battle.Monster.Kind with
                            | Weak -> 0.75
                            | Medium -> 0.55
                        if rng.NextDouble() < baseChance then
                            battle <- battle |> addLog "You ran away safely!"
                            mode <- Explore
                            toast "Escaped!"
                            autosave()
                        else
                            battle <- battle |> addLog "You failed to run!"
                            doMonsterAttack()

                    // Keyboard shortcuts
                    if not battle.Ended then
                        if Raylib.IsKeyPressed(KeyboardKey.A) |> toBool then
                            doPlayerAttack()
                        if Raylib.IsKeyPressed(KeyboardKey.R) |> toBool then
                            tryRun()

                    // Mouse clicks (buttons)
                    if not battle.Ended && (Raylib.IsMouseButtonPressed(MouseButton.Left) |> toBool) then
                        let mp = Raylib.GetMousePosition()
                        let mx, my = int mp.X, int mp.Y

                        let panelW, panelH = 560, 480
                        let panelX, panelY = (winW - panelW)/2, (winH - panelH)/2
                        let btnW, btnH = 120, 44
                        let btnBarH = 90
                        let btnBarTop = panelY + panelH - btnBarH
                        let attackX, attackY = panelX + 40, btnBarTop + 22
                        let runX, runY = panelX + 180, btnBarTop + 22

                        let inside x y w h =
                            mx >= x && mx < x+w && my >= y && my < y+h

                        if inside attackX attackY btnW btnH then
                            doPlayerAttack()
                        elif inside runX runY btnW btnH then
                            tryRun()

            match mode with
            | Battle _ -> mode <- Battle battle
            | Explore -> ()

        // Camera follow + clamp
        let playerCenter = playerPos + Vector2(drawTileSizeF/2.0f, drawTileSizeF/2.0f)
        let halfViewW = cam.Offset.X / cam.Zoom
        let halfViewH = cam.Offset.Y / cam.Zoom

        let maxTx = max halfViewW (worldW - halfViewW)
        let maxTy = max halfViewH (worldH - halfViewH)

        let tx = clamp playerCenter.X halfViewW maxTx
        let ty = clamp playerCenter.Y halfViewH maxTy
        cam.Target <- Vector2(tx, ty)

        // Visible tile range
        let topLeftWorld  = cam.Target - (cam.Offset / cam.Zoom)
        let botRightWorld = cam.Target + (cam.Offset / cam.Zoom)

        let minX = max 0 (int (floor (topLeftWorld.X  / drawTileSizeF)) - 1)
        let minY = max 0 (int (floor (topLeftWorld.Y  / drawTileSizeF)) - 1)
        let maxX = min (mapW-1) (int (floor (botRightWorld.X / drawTileSizeF)) + 1)
        let maxY = min (mapH-1) (int (floor (botRightWorld.Y / drawTileSizeF)) + 1)

        // ------------------ DRAW ------------------
        Raylib.BeginDrawing()
        Raylib.ClearBackground(Color.RayWhite)

        Raylib.BeginMode2D(cam)

        for y in minY .. maxY do
            for x in minX .. maxX do
                let code = mapCodes[y, x]
                let tileIndex = mapCodeToTilesetIndex code

                let src = Rectangle(float32 tileIndex * tileSizeF, 0.0f, tileSizeF, tileSizeF)
                let dst = Rectangle(float32 (x * drawTileSize), float32 (y * drawTileSize), drawTileSizeF, drawTileSizeF)
                Raylib.DrawTexturePro(tex, src, dst, Vector2(0f,0f), 0.0f, Color.White)

                if code = 8 then
                    let bx = x * drawTileSize
                    let by = y * drawTileSize
                    Raylib.DrawRectangle(bx+18, by+18, drawTileSize-36, drawTileSize-24, Color(180uy, 120uy, 80uy, 255uy))
                    Raylib.DrawRectangle(bx+22, by+22, drawTileSize-44, 22, Color(235uy, 235uy, 235uy, 255uy))
                    Raylib.DrawRectangle(bx+22, by+46, drawTileSize-44, drawTileSize-58, Color(90uy, 150uy, 240uy, 255uy))

        let pSrc = Rectangle(float32 TILE_PLAYER * tileSizeF, 0.0f, tileSizeF, tileSizeF)
        let pDst = Rectangle(playerPos.X, playerPos.Y, drawTileSizeF, drawTileSizeF)
        Raylib.DrawTexturePro(tex, pSrc, pDst, Vector2(0f,0f), 0.0f, Color.White)

        if mode = Explore && (not inventoryOpen) then
            let mouseWorld = Raylib.GetScreenToWorld2D(Raylib.GetMousePosition(), cam)
            let hover = worldToTile mouseWorld
            if inBounds hover then
                let hx = hover.x * drawTileSize
                let hy = hover.y * drawTileSize
                Raylib.DrawRectangle(hx, hy, drawTileSize, drawTileSize, Color(255uy,255uy,0uy,70uy))
                Raylib.DrawRectangleLines(hx, hy, drawTileSize, drawTileSize, Color.Yellow)

        Raylib.EndMode2D()

        // UI
        Raylib.DrawText($"Map: {currentMap}  (I=inventory)  (F5=save, F9=load)", 10, 10, 18, Color.Black)
        Raylib.DrawText($"Lv {hero.Level}  HP: {hero.HP}/{hero.MaxHP}  ATK:{effectiveATK hero}  DEF:{effectiveDEF hero}  XP:{hero.XP}/{hero.NextXP}", 10, 32, 18, Color.Black)

        if mode = Explore && inBounds playerTile && mapCodes[playerTile.y, playerTile.x] = 8 then
            Raylib.DrawText("Press E to sleep (full HP)", 10, 54, 18, Color.Black)

        if toastTimer > 0.0f then
            Raylib.DrawRectangle(8, winH-40, winW-16, 32, Color(0uy,0uy,0uy,160uy))
            Raylib.DrawText(toastText, 16, winH-34, 18, Color.RayWhite)

        // Inventory popup
        if inventoryOpen then
            let panelW, panelH = 360, 440
            let panelX, panelY = winW - panelW - 20, 20

            Raylib.DrawRectangle(panelX, panelY, panelW, panelH, Color(20uy, 20uy, 22uy, 220uy))
            Raylib.DrawRectangleLines(panelX, panelY, panelW, panelH, Color(230uy,230uy,230uy,200uy))

            Raylib.DrawText("Inventory (I)", panelX+20, panelY+16, 22, Color.RayWhite)

            Raylib.DrawText($"Level: {hero.Level}", panelX+20, panelY+56, 18, Color.RayWhite)
            Raylib.DrawText($"XP: {hero.XP}/{hero.NextXP}", panelX+20, panelY+78, 18, Color.RayWhite)
            Raylib.DrawText($"HP: {hero.HP}/{hero.MaxHP}", panelX+20, panelY+100, 18, Color.RayWhite)

            Raylib.DrawText($"ATK: {effectiveATK hero}", panelX+200, panelY+56, 18, Color.RayWhite)
            Raylib.DrawText($"DEF: {effectiveDEF hero}", panelX+200, panelY+78, 18, Color.RayWhite)

            let slotW, slotH = panelW - 40, 44

            let drawSlot (label:string) (itOpt:Item option) (x:int) (y:int) =
                Raylib.DrawRectangle(x, y, slotW, slotH, Color(40uy,40uy,45uy,220uy))
                Raylib.DrawRectangleLines(x, y, slotW, slotH, Color(200uy,200uy,200uy,160uy))
                let text =
                    match itOpt with
                    | None -> $"{label}: (empty)  [click]"
                    | Some it -> $"{label}: {itemName it}  {itemDesc it}  [click to unequip]"
                Raylib.DrawText(text, x+10, y+12, 16, Color.RayWhite)

            drawSlot "Weapon" hero.Equip.Weapon (panelX+20) (panelY+140)
            drawSlot "Armor"  hero.Equip.Armor  (panelX+20) (panelY+200)

            Raylib.DrawText("Bag (click to equip):", panelX+20, panelY+262, 18, Color.RayWhite)

            let listX = panelX + 20
            let mutable y = panelY + 290
            let rowH = 28

            hero.Bag
            |> List.truncate 10
            |> List.iter (fun it ->
                Raylib.DrawText($"- {itemName it}  {itemDesc it}", listX, y, 16, Color(235uy,235uy,235uy,230uy))
                y <- y + rowH)

        // Battle popup
        match mode with
        | Battle b ->
            Raylib.DrawRectangle(0, 0, winW, winH, Color(0uy,0uy,0uy,140uy))

            let panelW, panelH = 560, 480
            let panelX, panelY = (winW - panelW)/2, (winH - panelH)/2

            Raylib.DrawRectangle(panelX, panelY, panelW, panelH, Color(22uy,22uy,26uy,240uy))
            Raylib.DrawRectangleLines(panelX, panelY, panelW, panelH, Color(230uy,230uy,230uy,200uy))

            Raylib.DrawText("Encounter!", panelX+20, panelY+16, 26, Color.RayWhite)

            let fieldX, fieldY = panelX + 20, panelY + 54
            let fieldW, fieldH = panelW - 40, 170
            Raylib.DrawRectangle(fieldX, fieldY, fieldW, fieldH, Color(70uy,170uy,90uy,255uy))
            Raylib.DrawRectangleLines(fieldX, fieldY, fieldW, fieldH, Color(0uy,0uy,0uy,120uy))

            let cx = fieldX + fieldW/2
            let cy = fieldY + fieldH/2
            let r = 36.0f

            let mcol =
                match b.Monster.Kind with
                | Weak -> Color(90uy, 220uy, 120uy, 255uy)
                | Medium -> Color(220uy, 140uy, 90uy, 255uy)

            Raylib.DrawCircle(cx, cy, r, mcol)
            Raylib.DrawCircleLines(cx, cy, r, Color(0uy,0uy,0uy,200uy))
            Raylib.DrawText(b.Monster.Name, cx - (b.Monster.Name.Length*6), cy - 8, 18, Color.Black)

            let barX, barY = panelX + 20, fieldY + fieldH + 10
            let barW, barH = panelW - 40, 18
            Raylib.DrawRectangle(barX, barY, barW, barH, Color(60uy,60uy,60uy,255uy))
            let hpFrac = float32 b.Monster.HP / float32 b.Monster.MaxHP
            Raylib.DrawRectangle(barX, barY, int (float32 barW * hpFrac), barH, Color(230uy,70uy,70uy,255uy))
            Raylib.DrawRectangleLines(barX, barY, barW, barH, Color(0uy,0uy,0uy,180uy))
            Raylib.DrawText($"Monster HP: {b.Monster.HP}/{b.Monster.MaxHP}", barX, barY+22, 18, Color.RayWhite)

            let btnW, btnH = 120, 44
            let btnBarH = 90
            let btnBarTop = panelY + panelH - btnBarH

            let logTop = barY + 50
            let logBottom = btnBarTop - 8
            Raylib.DrawText("Log:", panelX+20, logTop, 18, Color.RayWhite)

            let lineH = 20
            let maxLines = max 1 ((logBottom - (logTop + 24)) / lineH)

            let visibleLog =
                b.Log
                |> List.rev
                |> List.truncate maxLines
                |> List.rev

            let mutable ly = logTop + 24
            for line in visibleLog do
                Raylib.DrawText(line, panelX+20, ly, 16, Color(235uy,235uy,235uy,230uy))
                ly <- ly + lineH

            Raylib.DrawLine(panelX+20, btnBarTop, panelX+panelW-20, btnBarTop, Color(255uy,255uy,255uy,60uy))

            let attackX, attackY = panelX + 40, btnBarTop + 22
            let runX, runY = panelX + 180, btnBarTop + 22

            let (attackColor, runColor) =
                if b.Ended then
                    (Color(90uy,90uy,90uy,255uy), Color(90uy,90uy,90uy,255uy))
                else
                    (Color(70uy,120uy,240uy,255uy), Color(120uy,120uy,120uy,255uy))

            Raylib.DrawRectangle(attackX, attackY, btnW, btnH, attackColor)
            Raylib.DrawRectangleLines(attackX, attackY, btnW, btnH, Color(0uy,0uy,0uy,200uy))
            Raylib.DrawText("Attack (A)", attackX+14, attackY+12, 18, Color.RayWhite)

            Raylib.DrawRectangle(runX, runY, btnW, btnH, runColor)
            Raylib.DrawRectangleLines(runX, runY, btnW, btnH, Color(0uy,0uy,0uy,200uy))
            Raylib.DrawText("Run (R)", runX+24, runY+12, 18, Color.RayWhite)

            if b.Ended then
                Raylib.DrawText("Press SPACE to respawn at bed", panelX+320, btnBarTop+34, 16, Color.RayWhite)

        | Explore -> ()

        Raylib.EndDrawing()

    Raylib.UnloadTexture(tex)
    Raylib.CloseWindow()
    0
    