module Game(startGame) where

  import Control.Monad
  import Data.Char
  import Draw

  type ErrorMessage = String
  type Cell = (Int, Int, Char)

  data Token = Token Char | X | O
  data Player = Player Token | Computer Token
  data Board = Board [Cell]
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
      board = Board []

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
  checkPosition :: Player -> Game -> (Bool, ErrorMessage)
  checkPosition = undefined

  -- go put user's token to a cell
  go :: Player -> Either (Game, GameError) Game
  go (Computer t) = Right (aoGo (Computer t))

  -- aiGo does step for AI
  aoGo :: Player -> Game
  aoGo (Computer t) = undefined

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

  promptMove :: IO()
  promptMove = do
    pos <- drawPrompt
    case convertStrToPosition pos of
      PositionUnsupported _ ->
        promptMove
      pos -> do
        drawToken Cross pos
        promptMove
