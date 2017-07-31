{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleContexts
    #-}

module Main where



import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

--import Control.Monad.Trans.State
import Control.Monad.State


import Control.Lens

import Control.Monad.IO.Class (liftIO)

import Control.Monad

import System.Exit (exitSuccess)



type GameMap = ([[Char]], (Int, Int))

mapex = (
      ["################"
      ,"#              #"
      ,"#              #"
      ,"#              #"
      ,"#              #"
      ,"################"
      ]
      , (16, 6)
      )

data Input = Up
           | Down 
           | Left
           | Right
           | Exit
           deriving (Eq, Show)

data Position = Position 
    { _x :: Int
    , _y :: Int
    } deriving (Show, Eq)

makeLenses ''Position

type Icon = Char

data Object = Object
    { _position :: Position
    , _icon :: Icon 
    } deriving Show
makeLenses ''Object

data Player = Player 
    { playerObject :: Object
    } deriving Show
$(makeFields ''Player)

data Enemy = Enemy
    { enemyObject :: Object 
    } deriving Show
$(makeFields ''Enemy)


p = Player (Object (Position 5 5) '@')

e = Enemy (Object (Position 5 5) '$')

--e & (object.position.x) .~ 155

--type App = StateT GameState IO


type GameState = Player

----------------------------
    --helper functions--
----------------------------

printMap :: GameMap -> IO ()
printMap (map, size) = setCursorPosition 0 0 >> mapM_ (putStrLn) map

updatePosition :: (HasObject a Object) => (Int, Int) -> a -> a
updatePosition (dx, dy) obj = obj & ((object.position.x) +~ dx) . ((object.position.y) +~ dy)

collide :: (HasObject a Object, HasObject b Object) => a -> b -> Bool
collide x y = x^.object.position == y^.object.position

printObject :: HasObject a Object => a -> IO ()
printObject obj = do
    setCursorPosition (obj^.object.position.x) (obj^.object.position.y)
    putChar $ obj^.object.icon

----------------------------



gameLoop :: StateT GameState IO ()
gameLoop = do
    lift clearScreen
    lift $ printMap mapex
    st <- get 
    lift . printObject $ st
    command <- (lift getInput)
    case command of
      Exit -> lift handleExit
      _    -> let dxdy = handleInput command in 
                  modify $ updatePosition dxdy 
    gameLoop

getInput :: IO Input
getInput = do
    char <- getChar
    case char of
      'w' -> return Up
      's' -> return Down
      'a' -> return Left
      'd' -> return Right
      'q' -> return Exit
      _   -> getInput

handleInput :: Input -> (Int, Int)
handleInput Up = (-1, 0)
handleInput Down = (1, 0)
handleInput Left = (0, -1)
handleInput Right = (0, 1)
handleInput _ = error "Whoops"

initialize :: IO ()
initialize = do
    clearScreen
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "My game"

drawScreen :: StateT GameState IO ()
drawScreen = do
    st <- get
    lift . printObject $ st


main = do
    initialize
    evalStateT gameLoop p

handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [ Reset ]
  putStrLn "Thank you for playing!"
  exitSuccess
