module Data.Request.Access.Methods
    ( groupActionsAccess
    , isFindAccess
    , findAction
    , iterateActions
    , isFindEssence
    , isAccess
    ) where

import Config

import Data.Request.Access

import Data.List
import Data.Text (Text)

type Action = Text
type Actions = [Action]
type Name = Text
type Names = [Name]

groupActionsAccess :: [(Action, Access)] -> [(Actions, Access)]
groupActionsAccess listOfPair =
    let
        sortArr = sortOn snd listOfPair
        groupArr = groupBy (\(x1,y1) (x2,y2) -> y1 == y2) sortArr
        unzipArr = map unzip groupArr
        resultArr = map (\(xs,(y:ys)) -> (xs,y)) unzipArr
    in resultArr

isFindAccess :: Access -> [Access] -> Bool
isFindAccess _      []             = False
isFindAccess access (accessX:rest) =
    if access == accessX
        then True
        else isFindAccess access rest

findAction :: Action -> [(Actions, Access)] -> Maybe Access
findAction _      []                       = Nothing
findAction action ((actions, access):rest) =
    case find (action==) actions of
        Just var -> Just access
        Nothing  -> findAction action rest

iterateActions :: Name -> Action -> [Access] -> Api -> Bool
iterateActions essence action accessArr api =
    let
        listOfActionsAccess = getActionsAccess essence api
        actionsAccessArr = groupActionsAccess listOfActionsAccess
    in case findAction action actionsAccessArr of
        Just access -> isFindAccess access accessArr
        Nothing     -> False

isFindEssence :: Name -> Names -> Bool
isFindEssence _      []               = False
isFindEssence essence (essenceX:rest) =
    if essence == essenceX
        then True
        else isFindEssence essence rest

isAccess :: Name -> Action -> [Access] -> Api -> Bool
isAccess essence action accessArr api =
    let essenceArr = getEssences api
    in if isFindEssence essence essenceArr
        then iterateActions essence action accessArr api
        else False