import "world"

-- The random parameter 'r' is used to decrease the frequency of some interactions.
let applyAlchemy' (r: i32) (x: element_type) (y: element_type): (element_type, element_type) =
  match (x, y)
  -- water + salt = salt_water + nothing
  case (#water, #salt) -> (#salt_water, #nothing)
  case (#water, #salt) -> (#nothing, #salt_water)

  -- water + fire = steam + nothing
  case (#water, #fire) -> (#steam_water, #nothing)
  case (#fire, #water) -> (#nothing, #steam_water)

  -- salt water + fire = steam + salt
  case (#salt_water, #fire) -> (#steam_water, #salt)
  case (#fire, #salt_water) -> (#salt, #steam_water)

  -- oil + fire = new fire + new fire
  case (#oil, #fire) -> (#fire, #fire)
  case (#fire, #oil) -> (#fire, #fire)

  -- torch + nothing = torch + fire
  case (#torch, #nothing) -> (#torch, #fire)
  case (#nothing, #torch) -> (#fire, #torch)

   -- napalm + nothing = napalm + fire
  case (#napalm, #nothing) -> (#napalm, #fire)
  case (#nothing, #napalm) -> (#fire, #napalm)

  -- spout + nothing = spout + water
  case (#spout, #nothing) -> (#spout, #nothing)
  case (#nothing, #spout) -> (#nothing, #spout)

  -- fire + plant = new fire + sand OR new fire + new fire
  case (#fire, #plant) -> if r < 2000 then (#fire, #sand) else (#fire, #fire)
  case (#plant, #fire) -> if r < 2000 then (#sand, #fire) else (#fire, #fire)

  -- water + plant = plant + plant
  case (#water, #plant) -> (#plant, #plant)
  case (#plant, #water) -> (#plant, #plant)

  -- water + metal = water + sand
  case (#water, #metal) -> if r < 100 then (#water, #sand) else (x, y)
  case (#metal, #water) -> if r < 100 then (#sand, #water) else (x, y)

    -- salt_water + metal = salt_water + sand
  case (#salt_water, #metal) -> if r < 300 then (#salt_water, #sand) else (x, y)
  case (#metal, #salt_water) -> if r < 300 then (#sand, #salt_water) else (x, y)

  -- lava + stone/metal/sand/salt = 2 * lava
  case (#lava, #stone) -> if r < 500 then (#lava, #lava) else (x, y)
  case (#stone, #lava) -> if r < 500 then (#lava, #lava) else (x, y)

  case (#lava, #metal) -> if r < 100 then (#lava, #lava) else (x, y)
  case (#metal, #lava) -> if r < 100 then (#lava, #lava) else (x, y)

  case (#lava, #sand) -> if r < 5000 then (#lava, #lava) else (x, y)
  case (#sand, #lava) -> if r < 5000 then (#lava, #lava) else (x, y)

  case (#lava, #salt) -> if r < 5000 then (#lava, #lava) else (x, y)
  case (#salt, #lava) -> if r < 5000 then (#lava, #lava) else (x, y)

  -- lava + oil/plant = lava + fire
  case (#lava, #oil) -> if r < 8000 then (#lava, #fire) else (x, y)
  case (#oil, #lava) -> if r < 8000 then (#fire, #lava) else (x, y)

  case (#lava, #plant) -> if r < 8000 then (#lava, #fire) else (x, y)
  case (#plant, #lava) -> if r < 8000 then (#fire, #lava) else (x, y)

  -- water + lava = steam + stone
  case (#water, #lava) -> (#steam_water, #stone)
  case (#lava, #water) -> (#stone, #steam_water)

  -- salt_water + lava = steam + stone OR steam + salt
  case (#salt_water, #lava) -> if r < 20000 then (#steam_water, #salt) else (#steam_water, #stone)
  case (#lava, #salt_water) -> if r < 20000 then (#salt, #steam_water) else (#stone, #steam_water)

  -- wall + steam = wall + condensed steam
  case (_, #steam_water) -> if isWall' x then (x, #steam_condensed) else (x,y)
  case (#steam_water, _) -> if isWall' y then (#steam_condensed, y) else (x,y)

  case _ -> (x, y)


let applyAlchemy (r: i32) (x: element) (y: element) : (element, element) =
  let (x', y') = applyAlchemy' r x.1 y.1
  in ((x', x.2), (y', y.2))
