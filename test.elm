import Keyboard
import Window
import Random
import Text 
-- Inputs


type Frog = {x: Float, y: Float, v: Float, dir: String, status: String}
frog = { x = 0, y = -175, v = 100, dir = "up", status = "alive" }

type Car = {x: Float, y: Float, vx: Float, color: String}
carRight = {x= -450, y= -80, vx= 100, color= "blue"}
carLeft = {x= 500, y= 20, vx= -100, color= "blue"}

type Rect = {left: Float, right: Float, top: Float, bottom: Float}

type Home = {x: Float , y: Float, status: String }
home ={x = 0, y = 225, status = "empty"}

type Game = {frogs: [Frog] , rightcars: [Car], leftcars: [Car], homes: [Home]}
game ={ frogs = [frog]
       , rightcars = []
       , leftcars = []
       , homes = (map (\i -> {home | x <- i}) [-400,-200,0,200,400])
       , status = "playing"}
phantomHomes = (map (\i -> {home | x <- i}) [-400,-200,0,200,400])
-- car appearance probility 
carProb = 0.05

blackRectangle = traced (solid black) square 

square = path [ (500, 300), (500, -200), (-500, -200), (-500, 300), (500, 300) ]

yellowLine = traced (solid  yellow) line 

line = path [ (500, 0), (-500, 0)]

rectangle colr w h (x, y) = 
  move (x , y)
    (group [ filled colr (rect w h)])

carColor : [String]
carColor = ["aqua", "blue", "green", "red", "white", "yellow"]
getItem : Int -> [String] ->String
getItem i lst = 
    if i == 0 then (head lst) else (getItem (i-1) (tail lst))

forward {x,y} object = 
  let obj = head object
      update_obj = if | y > 0 && obj.dir == "left" -> {obj | x <- max -400 (obj.x - obj.v)} 
                      | y > 0 && obj.dir == "right" -> {obj | x <- min 400 (obj.x + obj.v)}
                      | y > 0 && obj.dir == "up" -> {obj | y <- min 215 (obj.y + obj.v)} 
                      | y > 0 && obj.dir == "down" -> {obj | y <- max -175 (obj.y - obj.v)} 
                      | otherwise -> obj
  in update_obj :: (tail object)

rotateFrog {x,y} object =
  let obj = head object
      update_obj = {obj | dir <- if | x < 0 && obj.dir == "right" -> "up"
                                    | x < 0 && obj.dir == "up" -> "left"
                                    | x > 0 && obj.dir == "right" -> "down"
                                    | x > 0 && obj.dir == "down" -> "left"
                                    | x > 0 && obj.dir == "left" -> "up"
                                    | x < 0 && obj.dir == "left" -> "down" 
                                    | x < 0 && obj.dir == "down" -> "right"
                                    | x > 0 && obj.dir == "up" -> "right"
                                    | otherwise -> obj.dir}
  in update_obj :: (tail object)


stepFrog keys frogs = 
  frogs |> rotateFrog keys |> forward keys

update_car : String ->[Car] -> [Car]
update_car dirc lst = 
  if lst == [] then []
    else
      let lastCar = last lst
          llst = length lst
      in if dirc == "right" 
           then if lastCar.x > 490 then (take (llst-1) lst) else lst
         else if lastCar.x < -460 then (take (llst-1) lst) else lst

stepRightCars : Float -> Float -> Int -> [Car] -> [Car]
stepRightCars dt rand01 randint05 carlst = 
  let forward = map (\c -> {c | x <- (c.x+c.vx*dt)}) carlst
      car_conflicted = any (\c -> c.x < (-240)) carlst
      curcarLst = update_car "right" forward
  in if (rand01 < carProb && car_conflicted == False) then {carRight |color<- (getItem randint05 carColor)}:: curcarLst
     else curcarLst

stepLeftCars : Float -> Float -> Int -> [Car] -> [Car]
stepLeftCars dt rand01 randint05 carlst = 
  let forward = map (\c -> {c | x <- (c.x+c.vx*dt)}) carlst
      car_conflicted = any (\c -> c.x > (240)) carlst
      curcarLst = update_car "left" forward
  in if (rand01 < carProb && car_conflicted == False) then {carLeft |color<- (getItem randint05 carColor)}:: curcarLst
     else curcarLst

intersectRect : Rect->Rect->Bool
intersectRect r1 r2 = not (r2.left > r1.right || r2.right < r1.left || r2.top > r1.bottom || r2.bottom < r1.top)

cartoRectCoord : Car -> Rect
cartoRectCoord c = {left= (c.x-80/2), right=(c.x+80/2) , top= (c.y-50/2), bottom = (c.y+50/2)}

frogtoRectCoord : Frog -> Rect
frogtoRectCoord f = {left= (f.x-20), right=(f.x+20) , top= (f.y-20), bottom = (f.y+20)}

hometoRectCoord : Home -> Rect
hometoRectCoord h = {left= (h.x-20), right=(h.x+20) , top= (h.y-20), bottom = (h.y+20)}

step (dt, keys, lrand01, lrandint05, rrand01, rrandint05) hero =
  let cfrogs = stepFrog keys hero.frogs
      crightcars = stepRightCars dt rrand01 rrandint05 hero.rightcars
      cleftcars = stepLeftCars dt lrand01 lrandint05 hero.leftcars
      chomes = hero.homes
      cstatus = hero.status
      curFrog = head cfrogs
      curFrogPos = frogtoRectCoord curFrog
      rightcurCarsPos = map cartoRectCoord crightcars
      rightdead = any (\c -> (intersectRect curFrogPos c)) rightcurCarsPos
      leftcurCarsPos = map cartoRectCoord cleftcars
      leftdead = any (\c -> (intersectRect curFrogPos c)) leftcurCarsPos
      homePos = map hometoRectCoord chomes
      isgethome = any (\c -> (intersectRect curFrogPos c)) homePos

  in if (length cfrogs) == 6 then          
                    {hero | frogs <- (cfrogs)
                           ,rightcars <- crightcars
                           ,leftcars <- cleftcars
                           ,homes <- chomes
                           ,status <- "over"}
     else if (leftdead == True || rightdead == True) then 
              let df = ({curFrog | status <- "dead"}::[])++(tail cfrogs)
              in  { hero | frogs <- (frog::df)
                           ,rightcars <- crightcars
                           ,leftcars <- cleftcars
                           ,homes <- chomes
                           ,status <- cstatus}
          else if (isgethome==True) then
                   let detectGetHome curfp home = intersectRect curfp (hometoRectCoord home)
                       dh = filter (\c -> (not (detectGetHome curFrogPos c))) chomes 
                   in if (isEmpty dh) then 
                          {hero | frogs <- (cfrogs)
                                  ,rightcars <- crightcars
                                  ,leftcars <- cleftcars
                                  ,homes <- dh
                                  ,status <- "win"}
                      else 
                         { hero | frogs <- (frog::cfrogs)
                           ,rightcars <- crightcars
                           ,leftcars <- cleftcars
                           ,homes <- dh
                           ,status <- cstatus}
                else {hero | frogs <- (cfrogs)
                            ,rightcars <- crightcars
                            ,leftcars <- cleftcars
                            ,homes <- chomes
                            ,status <- cstatus}
render (w', h') ({frogs, rightcars, leftcars, homes, status} as hero) =
  let formFrog fg = toForm (image 40 40 (if fg.status == "dead" then "/imgs/splat.gif" else "/imgs/frog-" ++ fg.dir ++ ".png")) |> move (fg.x, fg.y)
      formHome he = toForm (image 40 40 "/imgs/house.png") |> move (he.x, he.y)
      formLcars l = toForm (image 80 50 ("/imgs/" ++l.color++ "-car-left.png")) |> move (l.x, l.y)
      formRcars r = toForm (image 80 50 ("/imgs/" ++ r.color++ "-car-right.png")) |> move (r.x, r.y)
      x = if hero.status == "over" || hero.status == "win" 
                                   then [rectangle grey 1025 200 (10, -30),  move (10, -30) blackRectangle, move (10, -30) yellowLine]
                                        ++[toForm (centered (monospace (Text.height 40 (Text.color red (bold (toText "GAME OVER!"))))))]
                                   else [rectangle grey 1025 200 (10, -30),  move (10, -30) blackRectangle, move (10, -30) yellowLine] 
                                        ++ map formFrog frogs ++ map formLcars leftcars ++ map formHome homes ++ map formRcars rightcars ++ map formHome phantomHomes     
  in  container  w' h' middle (collage  1025 625 x)
-- MARIO delta Keyboard.arrows
input = let delta = inSeconds<~ fps 8
        in  sampleOn delta (lift6 (,,,,,) delta Keyboard.arrows (Random.float delta) 
                            (Random.range 0 5 delta) (Random.float delta) (Random.range 0 5 delta))

main = lift2 render Window.dimensions (foldp step game input)