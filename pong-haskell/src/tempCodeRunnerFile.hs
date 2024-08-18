-- Lida com eventos
handleEvent :: Event -> Game -> Game
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) game
  | gameState game == Play =
    let (PaddleState (px, py) _) = playerPaddle game
        newPlayerPaddle = PaddleState (px, py) (-400)  -- Velocidade para a esquerda
    in game { playerPaddle = newPlayerPaddle }
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) game
  | gameState game == Play =
    let (PaddleState (px, py) _) = playerPaddle game
        newPlayerPaddle = PaddleState (px, py) 0  -- Para o movimento
    in game { playerPaddle = newPlayerPaddle }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) game
  | gameState game == Play =
    let (PaddleState (px, py) _) = playerPaddle game
        newPlayerPaddle = PaddleState (px, py) 400  -- Velocidade para a direita
    in game { playerPaddle = newPlayerPaddle }
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) game
  | gameState game == Play =
    let (PaddleState (px, py) _) = playerPaddle game
        newPlayerPaddle = PaddleState (px, py) 0  -- Para o movimento
    in game { playerPaddle = newPlayerPaddle }
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game
  | gameState game == Menu =
    -- Área de clique para "Play"
    if y > 10 && y < 70 && x > -80 && x < 80
    then game { gameState = Play, ballState = BallState (0, 0) (50, 400), initialBallSpeed = 400, gameTime = 0 }
    -- Área de clique para "Exit"
    else if y > -90 && y < -30 && x > -80 && x < 80
    then unsafePerformIO exitSuccess  -- Fecha o jogo
    else game
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game
  | gameState game == Play || gameState game == Paused =
    if y > 250 && y < 290 then
      if x > -340 && x < -260
        then initialState { gameState = Menu }  -- Menu
      else if x > -40 && x < 40
        then game { gameState = if gameState game == Play then Paused else Play }  -- Pause/Resume
      else if x > 260 && x < 340
        then initialState { gameState = Play }  -- Restart
      else game
    else game
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game
  | gameState game == Win || gameState game == Lose =
    if y > 250 && y < 290 then
      if x > -340 && x < -260
        then initialState { gameState = Menu }  -- Menu
      else if x > 260 && x < 340
        then initialState { gameState = Play }  -- Restart
      else game
    else game
handleEvent _ game = game