{-# LANGUAGE TemplateHaskell
           , MultiParamTypeClasses
           , FunctionalDependencies
    #-}

module Objects where

import Control.Lens

data Player = Player 
    { playerPosition :: Position
    } deriving Show

data Enemy = Enemy
    { enemyPosition :: Position
    } deriving Show

data Position = Position 
    { _x :: Int
    , _y :: Int
    } deriving Show

$(makeFields ''Player)
$(makeFields ''Enemy)
makeLenses ''Position

