{-
Jogo "Bola de Fogo e Bola de Agua"
Feito por Alexsandro Vítor Serafim de Carvalho <avsc@cin.ufpe.br>
-}

module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

--Pontos vermelho, azul
data GameAttribute = GA Int Int

instance Show GameAttribute where
 show (GA r b) = ((show r) ++ "/3 vermelhas, " ++ (show b) ++ "/3 azuis, " ++ (show (r+b)) ++ "/6 ao todo") 

--Int: Nível do jogo
--Bool: Já atualizou?
data GameState = Level Int Bool | GameOver | Finished

instance Show GameState where
 show (Level i _) = "Nivel " ++ show i
 show (GameOver) = "Voce perdeu"
 show (Finished) = "Voce venceu"

--Variaveis de tela
telaX = 960
telaY = 640
dTelaX = fromIntegral telaX :: GLdouble
dTelaY = fromIntegral telaY :: GLdouble
ladoIcone = 32

--Variaveis das bolas
speed = 5
jump = 12
fall = 1

--Variaveis das gemas
gemBounds = [(0,8), (-7,4), (-7,-4), (0,-8), (7,-4), (7,4)]

boundPos (xA, yA) (xB, yB) = ([(xB-xA, yB-yA), (xA-xB, yB-yA), (xB-xA, yA-yB), (xA-xB, yA-yB)], ((xA+xB)/2, (yA+yB)/2))
genBounds (x, y) = [(16*x, 16*y), ((-16)*x, 16*y), ((-16)*x, (-16)*y), (16*x, (-16)*y)]
coordXY (x, y) = (x*ladoIcone + 16, y*ladoIcone + 16)

main :: IO()
main = do
  --Tela do jogo
  let winConfig = ((50, 50), (telaX, telaY), "Bola de Fogo, Bola de Agua e o tesouro da praia")

  let gameMap = textureMap 0 dTelaX dTelaY dTelaX dTelaY

  --Grupos de objetos do jogo
  let objects = [(objectGroup "ballGroup" [createRed, createBlue]),
                 (objectGroup "blockGroup" [createFloor, createFloor2, createFloor3, createFloor4, createFloor5, createFloor6, createFloor7, createFloor8]),
                 (objectGroup "gemGroup" [createRedGem, createBlueGem]),
                 (objectGroup "portalGroup" [createPortal, createLava, createWater, createWater2, createMud, createMud2])
                ]

  --Nível inicial do jogo
  let startLevel = Level 1 True

  --Estado inicial do jogo (pontuacao 0)
  let initScore = GA 0 0

  --Controles do jogo
  let input = [(SpecialKey KeyLeft, StillDown, holdLeft),
               (SpecialKey KeyRight, StillDown, holdRight),
               (SpecialKey KeyUp, Press, pressUp),
               (Char 'a', StillDown, holdA),
               (Char 'd', StillDown, holdD),
               (Char 'w', Press, pressW),
               (Char 'o', Press, pressO)]

  --Lista de imagens
  let bmpList = [("praia1.bmp"           , Nothing), --0
                 ("areia30x1.bmp", Nothing), --1
                 ("areia1x2.bmp" , Nothing), --2
                 ("areia7x1.bmp" , Nothing)] --3

  funInit winConfig gameMap objects startLevel initScore input gameCycle (Timer 20) bmpList

--ballGroup: Personagens Jogáveis
createRed :: GameObject ()
createRed =
 let redBall = Basic (Circle 10.0 1.0 0.4 0.4 Filled)
 in object "redBall" redBall False (125,40) (0,0) ()

createBlue :: GameObject ()
createBlue =
 let blueBall = Basic (Circle 10.0 0.4 0.4 1.0 Filled)
 in object "blueBall" blueBall False (125,40) (0,0) ()

--blockGroup: Paredes
createFloor :: GameObject ()
createFloor =
 let block = Tex (30*ladoIcone, 1*ladoIcone) 1
 in object "floor" block False (coordXY (14.5,0)) (0,0) ()

createFloor2 :: GameObject ()
createFloor2 =
 let block = Tex (1*ladoIcone, 2*ladoIcone) 2
 in object "floor2" block False (coordXY (28,1.5)) (0,0) ()

createFloor3 :: GameObject ()
createFloor3 =
 let block = Tex (7*ladoIcone, 1*ladoIcone) 3
 in object "floor3" block False (coordXY (22,4)) (0,0) ()

createFloor4 :: GameObject ()
createFloor4 =
 let block = Tex (7*ladoIcone, 1*ladoIcone) 3
 in object "floor4" block False (coordXY (13,4)) (0,0) ()

createFloor5 :: GameObject ()
createFloor5 =
 let block = Tex (7*ladoIcone, 1*ladoIcone) 3
 in object "floor5" block False (coordXY (4,4)) (0,0) ()

createFloor6 :: GameObject ()
createFloor6 =
 let block = Tex (1*ladoIcone, 2*ladoIcone) 2
 in object "floor6" block False (coordXY (28,1.5)) (0,0) ()

createFloor7 :: GameObject ()
createFloor7 =
 let block = Tex (1*ladoIcone, 2*ladoIcone) 2
 in object "floor7" block False (coordXY (28,1.5)) (0,0) ()

createFloor8 :: GameObject ()
createFloor8 =
 let block = Tex (1*ladoIcone, 2*ladoIcone) 2
 in object "floor8" block False (coordXY (28,1.5)) (0,0) ()

--gemGroup: Objetos coletáveis
createRedGem :: GameObject ()
createRedGem =
 let gem = Basic (Polyg gemBounds 1.0 0.0 0.0 Filled)
     in object "redGem" gem False (coordXY (17.5, 5)) (0,0) ()

createBlueGem :: GameObject ()
createBlueGem =
 let gem = Basic (Polyg gemBounds 0.0 0.0 1.0 Filled)
     in object "blueGem" gem False (coordXY (29, 1)) (0,0) ()

--portalGroup: Portal para a proxima fase
createPortal :: GameObject ()
createPortal =
 let bounds = [(16, 16), (0, 24), (-16, 16), (-16, -16), (16, -16)]
     portal = Basic (Polyg bounds 0.3 0.3 0.3 Filled)
     in object "portal" portal False (coordXY (13,1)) (0,0) ()

createLava :: GameObject ()
createLava =
 let bounds = genBounds (2, 0.5)
     lava = Basic (Polyg bounds 1.0 0.3 0.0 Filled)
     in object "lava" lava False (coordXY (17.5,4)) (0,0) ()

createWater :: GameObject ()
createWater =
 let bounds = genBounds (2, 0.5)
     water = Basic (Polyg bounds 0.0 0.3 1.0 Filled)
     in object "water" water False (coordXY (8.5,4)) (0,0) ()

createWater2 :: GameObject ()
createWater2 =
 let bounds = genBounds (1, 0.5)
     water = Basic (Polyg bounds 0.0 0.3 1.0 Filled)
     in object "water2" water False (coordXY (29,2)) (0,0) ()

createMud :: GameObject ()
createMud =
 let bounds = genBounds (8, 0.5)
     mud = Basic (Polyg bounds 0.0 0.3 0.0 Filled)
     in object "mud" mud False (coordXY (14.5, 20)) (0,0) ()

createMud2 :: GameObject ()
createMud2 =
 let bounds = genBounds (30, 0.5)
     mud = Basic (Polyg bounds 0.0 0.3 0.0 Filled)
     in object "mud2" mud False (coordXY (14.5, 20)) (0,0) ()

gameCycle :: IOGame GameAttribute () GameState () ()
gameCycle = do
 --Impressões na tela
 level <- getGameState
 printOnScreen (show level) Helvetica18 (0, dTelaY - 18) 0.0 0.0 0.0
 gems <- getGameAttribute
 printOnScreen (show gems) Helvetica18 (0, dTelaY - 36) 0.0 0.0 0.0

 --Mudança de fase, só faz isso uma vez
 case (level) of
  GameOver -> gameOver
  Level 1 True -> level1
  Level 2 True -> level2
  Level 3 True -> level3
  Finished -> gameFinished
  Level _ False -> keepLevel
 showFPS TimesRoman24 (0,0) 1.0 1.0 0.0

 --Colisoes dos personagens
 red <- findObject "redBall" "ballGroup"
 manageCollisions red
 (pX, pY) <- getObjectPosition red
 blue <- findObject "blueBall" "ballGroup"
 manageCollisions blue
 (pX, pY) <- getObjectPosition blue

 --Contato com as gemas
 gema <- findObject "redGem" "gemGroup"
 col <- objectsCollision red gema
 when (col) (do
  (GA r b) <- getGameAttribute
  setGameAttribute (GA (r+1) b)
  setObjectPosition (coordXY (0, 17)) gema)
 gema <- findObject "blueGem" "gemGroup"
 col <- objectsCollision blue gema
 when (col) (do
  (GA r b) <- getGameAttribute
  setGameAttribute (GA r (b+1))
  setObjectPosition (coordXY (1, 17)) gema)

 --Ativação do portal para mudar de fase
 portal <- findObject "portal" "portalGroup"
 colRed <- objectsCollision red portal
 colBlue <- objectsCollision blue portal
 when (colRed && colBlue) (do
  level <- getGameState
  case (level) of
   Level 1 _ -> (setGameState (Level 2 True))
   Level 2 _ -> (setGameState (Level 3 True))
   Level 3 _ -> (setGameState Finished)
   Finished -> (setGameState Finished))

 --Se cair na lava
 portal <- findObject "lava" "portalGroup"
 col <- objectsCollision blue portal
 when (col) (setGameState GameOver)

 --Se cair na agua
 portal <- findObject "water" "portalGroup"
 col <- objectsCollision red portal
 portal <- findObject "water2" "portalGroup"
 col2 <- objectsCollision red portal
 when (col || col2) (setGameState GameOver)

 --Se cair na lama
 portal <- findObject "mud" "portalGroup"
 col <- objectsCollision red portal
 col2 <- objectsCollision blue portal
 when (col || col2) (setGameState GameOver)
 portal <- findObject "mud2" "portalGroup"
 col <- objectsCollision red portal
 col2 <- objectsCollision blue portal
 when (col || col2) (setGameState GameOver)

manageCollisions :: GameObject () -> IOGame GameAttribute () GameState () ()
manageCollisions bola = do
 --Colisao lateral
 colL <- objectLeftMapCollision bola
 colR <- objectRightMapCollision bola
 (_, sY) <- getObjectSpeed bola
 when (colL || colR) (setObjectSpeed (0, sY) bola)

 --Colisao vertical
 colB <- objectBottomMapCollision bola
 (sX, sY) <- getObjectSpeed bola
 (pX, pY) <- getObjectPosition bola
 (rad, _) <- getObjectSize bola
 setObjectSpeed (sX, sY - fall) bola
 when (colB) (do 
  setObjectSpeed (sX, 0) bola
  setObjectPosition (pX, rad/2) bola)

 --Colisao com bloco
 bloco <- findObject "floor" "blockGroup"
 collisionBallBlock bola bloco
 bloco <- findObject "floor2" "blockGroup"
 collisionBallBlock bola bloco
 bloco <- findObject "floor3" "blockGroup"
 collisionBallBlock bola bloco
 bloco <- findObject "floor4" "blockGroup"
 collisionBallBlock bola bloco
 bloco <- findObject "floor5" "blockGroup"
 collisionBallBlock bola bloco
 bloco <- findObject "floor6" "blockGroup"
 collisionBallBlock bola bloco
 bloco <- findObject "floor7" "blockGroup"
 collisionBallBlock bola bloco
 bloco <- findObject "floor8" "blockGroup"
 collisionBallBlock bola bloco

 --Se afundar
 if (pY < 0) then do 
  setObjectPosition (pX, 50) bola
  setObjectSpeed (0, -1) bola
 else setObjectPosition (pX, pY) bola

--Colisão bola com bloco
collisionBallBlock :: GameObject () -> GameObject () -> IOGame GameAttribute () GameState () ()
collisionBallBlock bola bloco = do
 (pX, pY) <- getObjectPosition bola
 (_, rad) <- getObjectSize bola
 (sX, sY) <- getObjectSpeed bola
 (_, yBloco) <- getObjectPosition bloco
 (_, tamBloco) <- getObjectSize bloco
 colBloco <- objectsCollision bola bloco
 when (colBloco) (do
  setObjectSpeed (0, 0) bola
  if (sY < 0 && pY > (yBloco + tamBloco/2))
   then setObjectPosition (pX, rad/2 + tamBloco/2 + yBloco - 0.1) bola
   else setObjectPosition (pX, yBloco - (tamBloco + rad)/2) bola
  )

--Montagem dos níveis do jogo
gameOver :: IOGame GameAttribute () GameState () ()
gameOver = do
 printOnScreen ("Uma das bolas morreu. Fim de jogo para voce.") TimesRoman24 (200, 3*dTelaY/4) 0.0 0.0 0.0
 printOnScreen ("Aperte O para recomecar o jogo.") TimesRoman24 (200, 3*dTelaY/4 - 24) 0.0 0.0 0.0

level1 :: IOGame GameAttribute () GameState () ()
level1 = do
 bola <- findObject "redBall" "ballGroup"
 setObjectPosition (125, 40) bola
 bola <- findObject "blueBall" "ballGroup"
 setObjectPosition (125, 40) bola
 bloco <- findObject "floor" "blockGroup"
 setObjectPosition (coordXY (14.5, 0)) bloco
 bloco <- findObject "floor2" "blockGroup"
 setObjectPosition (coordXY (28, 1.5)) bloco
 bloco <- findObject "floor3" "blockGroup"
 setObjectPosition (coordXY (22, 4)) bloco
 bloco <- findObject "floor4" "blockGroup"
 setObjectPosition (coordXY (13, 4)) bloco
 bloco <- findObject "floor5" "blockGroup"
 setObjectPosition (coordXY (4, 4)) bloco
 bloco <- findObject "floor6" "blockGroup"
 setObjectPosition (coordXY (28, 1.5)) bloco
 bloco <- findObject "floor7" "blockGroup"
 setObjectPosition (coordXY (28, 1.5)) bloco
 bloco <- findObject "floor8" "blockGroup"
 setObjectPosition (coordXY (28, 1.5)) bloco
 gema <- findObject "redGem" "gemGroup"
 setObjectPosition (coordXY (17.5, 5)) gema
 gema <- findObject "blueGem" "gemGroup"
 setObjectPosition (coordXY (29, 1)) gema
 portal <- findObject "portal" "portalGroup"
 setObjectPosition (coordXY (2, 5)) portal
 portal <- findObject "lava" "portalGroup"
 setObjectPosition (coordXY (17.5,4)) portal
 portal <- findObject "water" "portalGroup"
 setObjectPosition (coordXY (8.5,4)) portal
 portal <- findObject "water2" "portalGroup"
 setObjectPosition (coordXY (29,0.5)) portal
 portal <- findObject "mud" "portalGroup"
 setObjectPosition (coordXY (14.5,20)) portal
 setObjectSpeed (0,0) portal
 portal <- findObject "mud2" "portalGroup"
 setObjectPosition (coordXY (14.5,20)) portal
 setGameState (Level 1 False)

level2 :: IOGame GameAttribute () GameState () ()
level2 = do
 bola <- findObject "redBall" "ballGroup"
 setObjectPosition (coordXY (14.5,10)) bola
 bola <- findObject "blueBall" "ballGroup"
 setObjectPosition (coordXY (14.5,10)) bola
 bloco <- findObject "floor2" "blockGroup"
 setObjectPosition (coordXY (14.5,8)) bloco
 bloco <- findObject "floor3" "blockGroup"
 setObjectPosition (coordXY (22,5)) bloco
 bloco <- findObject "floor4" "blockGroup"
 setObjectPosition (coordXY (7,5)) bloco
 bloco <- findObject "floor5" "blockGroup"
 setObjectPosition (coordXY (14.5,1)) bloco
 bloco <- findObject "floor6" "blockGroup"
 setObjectPosition (coordXY (14.5,7)) bloco
 bloco <- findObject "floor7" "blockGroup"
 setObjectPosition (coordXY (14.5,7)) bloco
 bloco <- findObject "floor8" "blockGroup"
 setObjectPosition (coordXY (14.5,7)) bloco
 gema <- findObject "redGem" "gemGroup"
 setObjectPosition (coordXY (19,6)) gema
 gema <- findObject "blueGem" "gemGroup"
 setObjectPosition (coordXY (10,6)) gema
 portal <- findObject "portal" "portalGroup"
 setObjectPosition (coordXY (14.5,2)) portal
 portal <- findObject "lava" "portalGroup"
 setObjectPosition (coordXY (11.5,8)) portal
 portal <- findObject "water" "portalGroup"
 setObjectPosition (coordXY (17.5,8)) portal
 portal <- findObject "water2" "portalGroup"
 setObjectPosition (coordXY (17.5,8)) portal
 portal <- findObject "mud" "portalGroup"
 setObjectPosition (coordXY (14.5,5)) portal
 setGameState (Level 2 False)

level3 :: IOGame GameAttribute () GameState () ()
level3 = do
 bola <- findObject "redBall" "ballGroup"
 setObjectPosition (coordXY (7.5,7)) bola
 bola <- findObject "blueBall" "ballGroup"
 setObjectPosition (coordXY (7.5,7)) bola
 bloco <- findObject "floor" "blockGroup"
 setObjectPosition (coordXY (0,1)) bloco
 bloco <- findObject "floor2" "blockGroup"
 setObjectPosition (coordXY (27,1.5)) bloco
 bloco <- findObject "floor3" "blockGroup"
 setObjectPosition (coordXY (7.5,6)) bloco
 bloco <- findObject "floor4" "blockGroup"
 setObjectPosition (coordXY (14.5,6)) bloco
 bloco <- findObject "floor5" "blockGroup"
 setObjectPosition (coordXY (21.5,6)) bloco
 bloco <- findObject "floor6" "blockGroup"
 setObjectPosition (coordXY (24,1)) bloco
 bloco <- findObject "floor7" "blockGroup"
 setObjectPosition (coordXY (21,1)) bloco
 bloco <- findObject "floor8" "blockGroup"
 setObjectPosition (coordXY (18,1)) bloco
 gema <- findObject "redGem" "gemGroup"
 setObjectPosition (coordXY (20,8)) gema
 gema <- findObject "blueGem" "gemGroup"
 setObjectPosition (coordXY (20,7)) gema
 portal <- findObject "portal" "portalGroup"
 setObjectPosition (coordXY (1,2)) portal
 portal <- findObject "lava" "portalGroup"
 setObjectPosition (coordXY (14.5,7)) portal
 portal <- findObject "water" "portalGroup"
 setObjectPosition (coordXY (14.5,8)) portal
 portal <- findObject "water2" "portalGroup"
 setObjectPosition (coordXY (14.5,8)) portal
 portal <- findObject "mud" "portalGroup"
 setObjectPosition (coordXY (-3,7)) portal
 setObjectSpeed (2,0) portal
 portal <- findObject "mud2" "portalGroup"
 setObjectPosition (coordXY (14.5,-0.5)) portal
 setGameState (Level 3 False)

gameFinished :: IOGame GameAttribute () GameState () ()
gameFinished = do 
 printOnScreen ("Parabens! Voce zerou o jogo!") TimesRoman24 (200, 3*dTelaY/4 + 24) 0.0 0.0 0.0
 pontos <- getGameAttribute
 printOnScreen ("Gemas coletadas: "++ (show pontos)) TimesRoman24 (200, 3*dTelaY/4) 0.0 0.0 0.0
 printOnScreen ("Aperte O para recomecar o jogo.") TimesRoman24 (200, 3*dTelaY/4 -24) 0.0 0.0 0.0

--Uma funcao que faz um objeto parado ficar parado, é usada para a fase não reiniciar constantemente
--E sim, eu preciso dessa funcao inutil
keepLevel :: IOGame GameAttribute () GameState () ()
keepLevel = do
 bloco <- findObject "floor2" "blockGroup"
 setObjectSpeed (0, 0) bloco

--Controles
holdLeft :: Modifiers -> Position -> IOGame GameAttribute () GameState () ()
holdLeft _ _ = do
 bola <- findObject "redBall" "ballGroup"
 moveBallLeft bola

holdA :: Modifiers -> Position -> IOGame GameAttribute () GameState () ()
holdA _ _ = do
 bola <- findObject "blueBall" "ballGroup"
 moveBallLeft bola

moveBallLeft :: GameObject () -> IOGame GameAttribute () GameState () ()
moveBallLeft bola = do
 (pX, pY) <- getObjectPosition bola
 (rad, _) <- getObjectSize bola
 if (pX - rad/2 >= 0) then (setObjectPosition ((pX - speed), pY) bola)
 else (setObjectPosition (rad/2, pY) bola)

holdRight :: Modifiers -> Position -> IOGame GameAttribute () GameState () ()
holdRight _ _ = do
 bola <- findObject "redBall" "ballGroup"
 moveBallRight bola

holdD :: Modifiers -> Position -> IOGame GameAttribute () GameState () ()
holdD _ _ = do
 bola <- findObject "blueBall" "ballGroup"
 moveBallRight bola

moveBallRight :: GameObject () -> IOGame GameAttribute () GameState () ()
moveBallRight bola = do
 (pX, pY) <- getObjectPosition bola
 (rad, _) <- getObjectSize bola
 if (pX + rad/2 <= dTelaX) then (setObjectPosition ((pX + speed), pY) bola)
 else (setObjectPosition (dTelaX - rad/2, pY) bola)

pressUp :: Modifiers -> Position -> IOGame GameAttribute () GameState () ()
pressUp _ _ = do
 bola <- findObject "redBall" "ballGroup"
 ballJump bola

pressW :: Modifiers -> Position -> IOGame GameAttribute () GameState () ()
pressW _ _ = do
 bola <- findObject "blueBall" "ballGroup"
 ballJump bola

ballJump :: GameObject () -> IOGame GameAttribute () GameState () ()
ballJump bola = do
 (pX, pY) <- getObjectPosition bola
 (sX, sY) <- getObjectSpeed bola
 (rad, _) <- getObjectSize bola
 col <- colliding bola
 if ((pY - rad) <= 0 || col) then (setObjectSpeed (sX, jump) bola)
 else (setObjectSpeed (sX, sY) bola)

colliding :: GameObject () -> IOGame GameAttribute () GameState () Bool
colliding bola = do
 bloco <- findObject "floor" "blockGroup"
 colFloor <- objectsCollision bola bloco
 bloco <- findObject "floor2" "blockGroup"
 colFloor2 <- objectsCollision bola bloco
 bloco <- findObject "floor3" "blockGroup"
 colFloor3 <- objectsCollision bola bloco
 bloco <- findObject "floor4" "blockGroup"
 colFloor4 <- objectsCollision bola bloco
 bloco <- findObject "floor5" "blockGroup"
 colFloor5 <- objectsCollision bola bloco
 bloco <- findObject "floor6" "blockGroup"
 colFloor6 <- objectsCollision bola bloco
 bloco <- findObject "floor7" "blockGroup"
 colFloor7 <- objectsCollision bola bloco
 bloco <- findObject "floor8" "blockGroup"
 colFloor8 <- objectsCollision bola bloco
 return (colFloor || colFloor2 || colFloor3 || colFloor4 || colFloor5 || colFloor6 || colFloor7 || colFloor8)

pressO :: Modifiers -> Position -> IOGame GameAttribute () GameState () ()
pressO _ _ = do
 setGameState (Level 1 True)
 setGameAttribute (GA 0 0)
