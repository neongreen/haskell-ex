module Game(startGame) where

  import Control.Monad
  import Data.Char
  import Data.List
  import Draw

  type ErrorMessage = String
  type TokenSymbol = Char
  type Cell = (Char, Int, TokenSymbol)
  type Board = [Cell]

  data Token = Token Char | X | O
  data Player = Player Token | Computer Token

  data GameError = WrongPosition | PositionIsBusy
  data Game = Game {
                  currentPlayer :: Player
                , players :: (Player, Player)
                , board :: Board
              }
              | GameEnded Player

  -- createGame makes a new game
  createGame :: Player -> Game
  createGame (Player (Token ch)) =
    let
      makeComputer 'X' = Computer O
      makeComputer 'O' = Computer X

      player = Player (Token ch)
      computer = makeComputer ch
      board = []

      in
        Game {
          currentPlayer = player
          , players = (player, computer)
          , board = board
        }

  startGame :: IO Game
  startGame = do
    choise <- promptPlayerType
    let
      game = createGame (Player (Token choise))
      in
        return (processingGame game)

  processingGame :: Game -> Game
  processingGame (GameEnded p) = GameEnded p

  -- checkPosition check's can would the player put
  checkPosition :: BoardPosition -> Game -> (Bool, ErrorMessage)
  checkPosition (Position x y) game =
    case foundedPositions of
      [] -> (True, "")
      [x] -> (False, "Selected position was busy")
    where
      foundedPositions = filter (\(pX, pY, _) -> pX == x && pY == y) (board game)

  -- searchPosition is looking for a free position
  searchPosition :: Game -> BoardPosition
  searchPosition game =
    undefined

  -- go
  go :: Player -> Either (Game, GameError) Game
  go (Computer t) = Right (aiGo (Computer t))
  go (Player t) = playerGo (Player t)

  -- aiGo does step for AI
  aiGo :: Player -> Game
  aiGo (Computer t) = undefined

  -- palyerGo asks player step about
  playerGo :: Player -> Either (Game, GameError) Game
  playerGo (Player t) =
    undefined

  promptPlayerType :: IO Char
  promptPlayerType = do
    putStr "\nChoise (x or o): "
    select <- getChar
    let choise = toUpper select
      in
      if choise == 'X' || choise == 'O' then
        return choise
      else
        promptPlayerType

  promptMove :: IO BoardPosition
  promptMove = do
    pos <- drawPrompt
    case convertStrToPosition pos of
      PositionUnsupported _ ->
        promptMove
      pos -> return pos
