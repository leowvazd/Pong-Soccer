module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)

-- Estado do jogo
data GameState = Menu | Play | Exit | Paused | Win | Lose deriving (Eq)

-- Removido: TwoBalls e ThreeBalls não estavam sendo utilizados

ballSpeedIncreaseRate :: Float
ballSpeedIncreaseRate = 0.01 -- Aumenta as unidades por segundo

-- Estado da bola
data BallState = BallState
  { ballPos :: (Float, Float) -- Posição da bola (x, y)
  , ballVel :: (Float, Float) -- Velocidade da bola (vx, vy)
  } deriving (Eq)

-- Estado do disco
data PaddleState = PaddleState
  { paddlePos :: (Float, Float) -- Posição do disco (x, y)
  , paddleVel :: Float          -- Velocidade horizontal do disco
  } deriving (Eq)

-- Estado do jogo completo
data Game = Game
  { gameState :: GameState
  , ballState :: BallState
  , playerPaddle :: PaddleState
  , cpuPaddle :: PaddleState
  , gameTime :: Float
  , initialCpuSpeed :: Float
  , initialBallSpeed :: Float
  , playerScore :: Int
  , cpuScore :: Int
  } deriving (Eq)

-- Configurações da janela
window :: Display
window = InWindow "Pong - Soccer" (800, 600) (100, 100)

-- Cor de fundo
background :: Color
background = black

-- Estado inicial do jogo
initialState :: Game
initialState = Game
  { gameState = Menu
  , ballState = BallState (0, 0) (50, 400)
  , playerPaddle = PaddleState (0, -200) 0
  , cpuPaddle = PaddleState (0, 200) 0
  , gameTime = 0
  , initialCpuSpeed = 600
  , initialBallSpeed = 400
  , playerScore = 0
  , cpuScore = 0
  }

-- Função principal
main :: IO ()
main = play window background 60 initialState render handleEvent update

-- Função para desenhar o título com cores diferentes
drawTitle :: Picture
drawTitle = pictures
  [ translate (-200) 180 $ color (makeColor 1 0.5 0.5 1) $ scale 1 1 $ text "P"
  , translate (-170) 180 $ color (makeColor 0.5 1 0.5 1) $ scale 1 1 $ text "o"
  , translate (-140) 180 $ color (makeColor 0.5 0.5 1 1) $ scale 1 1 $ text "n"
  , translate (-110) 180 $ color (makeColor 1 1 0.5 1) $ scale 1 1 $ text "g"
  , translate (-080) 180 $ color (makeColor 1 0.5 1 1) $ scale 1 1 $ text " "
  , translate (-50) 180 $ color (makeColor 1 1 1 1) $ scale 1 1 $ text "-"
  , translate (-20) 180 $ color (makeColor 1 1 1 1) $ scale 1 1 $ text " "
  , translate (10) 180 $ color (makeColor 0.5 1 1 1) $ scale 1 1 $ text "S"
  , translate (40) 180 $ color (makeColor 1 0.5 1 1) $ scale 1 1 $ text "o"
  , translate (70) 180 $ color (makeColor 1 1 0.5 1) $ scale 1 1 $ text "c"
  , translate (100) 180 $ color (makeColor 0.5 1 0.5 1) $ scale 1 1 $ text "c"
  , translate (130) 180 $ color (makeColor 1 0.5 0.5 1) $ scale 1 1 $ text "e"
  , translate (160) 180 $ color (makeColor 0.5 1 1 1) $ scale 1 1 $ text "r"
  ]

-- Função para renderizar um fundo preto (limpar a tela)
renderBlackBackground :: Picture
renderBlackBackground = color black $ rectangleSolid 800 600

-- Renderiza o estado do jogo
render :: Game -> Picture
render game = case gameState game of
  Menu -> renderMenu
  Play -> renderPlayState game
  Paused -> renderPausedState game
  Exit -> blank
  Win -> renderWinScreen
  Lose -> renderLoseScreen

-- Renderiza o menu principal
renderMenu :: Picture
renderMenu = pictures 
  [ drawTitle
  , color red $ translate 0 40 $ rectangleWire 160 60
  , color red $ translate 0 (-60) $ rectangleWire 160 60
  , translate (-35) 30 $ scale 0.3 0.3 $ color white $ text "Play"
  , translate (-35) (-70) $ scale 0.3 0.3 $ color white $ text "Exit"
  ]

-- Renderiza o estado de jogo ativo
renderPlayState :: Game -> Picture
renderPlayState game = pictures 
  [ renderField
  , renderBall (ballState game)
  , renderPaddle (playerPaddle game)
  , renderPaddle (cpuPaddle game)
  , renderScore (playerScore game) (cpuScore game)
  , renderPauseButton
  , renderMenuButton
  , renderRestartButton
  ]

-- Renderiza o estado pausado
renderPausedState :: Game -> Picture
renderPausedState game = pictures 
  [ renderPlayState game
  , translate (-40) 260 $ scale 0.2 0.2 $ color green $ text "Pause"
  ]

-- Renderiza a tela de vitória
renderWinScreen :: Picture
renderWinScreen = pictures
  [ renderBlackBackground  -- Limpa a tela
  , translate (-120) 0 $ scale 0.5 0.5 $ color (makeColor 1 0.84 0 1) $ text "Win! :)" -- Color Gold
  , renderMenuButton
  , renderRestartButton
  ]

-- Renderiza a tela de derrota
renderLoseScreen :: Picture
renderLoseScreen = pictures
  [ renderBlackBackground  -- Limpa a tela
  , translate (-120) 0 $ scale 0.5 0.5 $ color (makeColor 0.75 0.75 0.75 1) $ text "Lose! :(" -- Color Silver
  , renderMenuButton
  , renderRestartButton
  ]

-- Renderiza o botão de Menu
renderMenuButton :: Picture
renderMenuButton = pictures
  [ color red $ translate (-300) 270 $ rectangleWire 100 40
  , translate (-340) 260 $ scale 0.2 0.2 $ color white $ text "Menu"
  ]

-- Renderiza o botão de Restart
renderRestartButton :: Picture
renderRestartButton = pictures
  [ color red $ translate (300) 270 $ rectangleWire 100 40
  , translate (260) 260 $ scale 0.2 0.2 $ color white $ text "Restart"
  ]

-- Renderiza o botão de Pause
renderPauseButton :: Picture
renderPauseButton = pictures
  [ color red $ translate 0 270 $ rectangleWire 100 40
  , translate (-40) 260 $ scale 0.2 0.2 $ color white $ text "Pause"
  ]

-- Renderiza o campo de jogo
renderField :: Picture
renderField = color white $ pictures
  [ translate 0 0 $ rectangleWire 600 450 -- Campo de jogo
  , circle 50 -- Círculo central
  , translate 0 0 $ rectangleWire 600 0 -- Linha central
  ]

-- Renderiza a bola
renderBall :: BallState -> Picture
renderBall (BallState (x, y) _) = translate x y $ color red $ circleSolid 10

-- Renderiza o disco (paddle)
renderPaddle :: PaddleState -> Picture
renderPaddle (PaddleState (x, y) _) = translate x y $ color white $ rectangleSolid 80 20

renderScore :: Int -> Int -> Picture
renderScore playerScore cpuScore = pictures
  [ translate (315) (-200) $ scale 0.5 0.5 $ color white $ text (show playerScore) -- Placar do jogador
  , translate (315) (170)$ scale 0.5 0.5 $ color white $ text (show cpuScore) -- Placar da CPU
  ]

-- Verifica se houve colisão entre a bola e o disco
checkPaddleCollision :: BallState -> PaddleState -> BallState
checkPaddleCollision (BallState (bx, by) (vx, vy)) (PaddleState (px, py) _) =
  let paddleHalfWidth = 40
      paddleHalfHeight = 10
      ballRadius = 10

      -- Calcula as bordas do disco e da bola
      paddleLeft = px - paddleHalfWidth
      paddleRight = px + paddleHalfWidth
      paddleTop = py + paddleHalfHeight
      paddleBottom = py - paddleHalfHeight

      ballLeft = bx - ballRadius
      ballRight = bx + ballRadius
      ballTop = by + ballRadius
      ballBottom = by - ballRadius

      -- Verifica colisão
      collided = ballRight > paddleLeft && ballLeft < paddleRight && ballTop > paddleBottom && ballBottom < paddleTop

      -- Calcula o ponto de colisão relativo ao centro do disco
      relativeCollisionPoint = (bx - px) / paddleHalfWidth

      -- Calcula o novo ângulo baseado no ponto de colisão
      newAngle = relativeCollisionPoint * (pi / 4)  -- Máximo de 45 graus

      -- Calcula a nova velocidade mantendo a magnitude original
      speed = sqrt (vx * vx + vy * vy)
      newVx = speed * sin newAngle
      newVy = if by > py then speed * cos newAngle else -(speed * cos newAngle)

  in if collided
     then BallState (bx, by) (newVx, newVy)
     else BallState (bx, by) (vx, vy)

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

-- Atualiza o estado do jogo
update :: Float -> Game -> Game
update seconds game@(Game Paused _ _ _ _ _ _ _ _) = game  -- Não atualiza se estiver pausado
update seconds game@(Game Win _ _ _ _ _ _ _ _) = game  -- Não atualiza se estiver na tela de vitória
update seconds game@(Game Lose _ _ _ _ _ _ _ _) = game  -- Não atualiza se estiver na tela de derrota
update seconds (Game Play ballState playerPaddle cpuPaddle gameTime initialCpuSpeed initialBallSpeed playerScore cpuScore) =
  let
    -- Limites do campo
    leftLimit = -300
    rightLimit = 300
    topLimit = 225
    bottomLimit = -225

    -- Calcula a velocidade atual da bola
    currentBallSpeed = initialBallSpeed + (gameTime * ballSpeedIncreaseRate)

    -- Atualiza a posição da bola
    (BallState (x, y) (vx, vy)) = ballState
    speedFactor = currentBallSpeed / initialBallSpeed
    newX = x + vx * seconds * speedFactor
    newY = y + vy * seconds * speedFactor

    -- Verifica colisão com as bordas laterais e inverte a direção se necessário
    (finalX, finalVx) = if newX - 10 < leftLimit || newX + 10 > rightLimit
                        then (x, -vx) else (newX, vx)
    
    -- Verifica se houve gol e atualiza o placar
    (finalY, finalVy, updatedPlayerScore, updatedCpuScore, resetPositions) = 
      if newY + 10 > topLimit 
      then (0, 200, playerScore + 1, cpuScore, True) -- Gol do jogador
      else if newY - 10 < bottomLimit
      then (0, -200, playerScore, cpuScore + 1, True) -- Gol da CPU
      else (newY, vy, playerScore, cpuScore, False) -- Nenhum gol

    -- Reseta as posições e velocidade se houve gol
    (updatedBallState, updatedPlayerPaddle, updatedCpuPaddle, updatedGameTime) = 
      if resetPositions
      then (BallState (0, 0) (if finalVx > 0 then -500 else 500, finalVy), 
            PaddleState (0, -200) 0, 
            PaddleState (0, 200) 0,
            30)  -- Reseta o tempo de jogo para reiniciar a aceleração da bola
      else
        let
          -- Atualiza a posição do disco do jogador
          (PaddleState (px, py) pv) = playerPaddle
          newPx = px + pv * seconds
          newPx' = max (leftLimit + 40) (min (rightLimit - 40) newPx) -- 40 é metade da largura do disco
          updatedPlayerPaddle' = PaddleState (newPx', py) pv

          -- Atualiza a posição e velocidade do disco da CPU
          (PaddleState (cpuX, cpuY) cpuVel) = cpuPaddle
          targetX = finalX  -- A CPU tenta seguir a posição X da bola
          
          -- Calcula a nova velocidade da CPU
          newCpuVel = if abs (targetX - cpuX) < 10
                      then 0  -- Parar se estiver próximo o suficiente
                      else if targetX > cpuX
                           then min initialCpuSpeed (cpuVel + initialCpuSpeed * seconds)  -- Acelerar para a direita
                           else max (-initialCpuSpeed) (cpuVel - initialCpuSpeed * seconds)  -- Acelerar para a esquerda
          
          -- Atualiza a posição da CPU
          newCpuX = cpuX + newCpuVel * seconds
          newCpuX' = max (leftLimit + 40) (min (rightLimit - 40) newCpuX)  -- Limitar a posição da CPU
          updatedCpuPaddle' = PaddleState (newCpuX', cpuY) newCpuVel

          -- Atualiza a posição da bola e dos discos
          updatedBallState' = BallState (finalX, finalY) (finalVx * speedFactor, finalVy * speedFactor)
          ballStateWithPlayerCollision = checkPaddleCollision updatedBallState' updatedPlayerPaddle'

          -- Verifica colisão com o disco da CPU
          finalBallState = checkPaddleCollision ballStateWithPlayerCollision updatedCpuPaddle'
        in (finalBallState, updatedPlayerPaddle', updatedCpuPaddle', gameTime + seconds)

    -- Verifica se algum jogador atingiu 3 gols
    newGameState = if updatedPlayerScore >= 3
                   then Win
                   else if updatedCpuScore >= 3
                        then Lose
                        else Play

  in Game newGameState updatedBallState updatedPlayerPaddle updatedCpuPaddle updatedGameTime initialCpuSpeed initialBallSpeed updatedPlayerScore updatedCpuScore
update _ game = game  -- Para outros estados (como Menu)