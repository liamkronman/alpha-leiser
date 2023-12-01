import AlphaZero.GI
using StaticArrays

const NUM_COLS = 8
const NUM_ROWS = 8
const NUM_SQUARES = NUM_COLS * NUM_ROWS


const Color = Bool
const WHITE = true
const BLACK = false

const PieceType = Bool
const PAWN = true
const MONARCH = false

@enum(MonarchDirection, N, W, S, E)
@enum(PawnDirection, NE, NW, SW, SE)
const Direction = Union{MonarchDirection, PawnDirection}
@enum(Rotation, RIGHT, UTURN, LEFT)
const SquareIdx = Int8

idx_of_xy((x, y)) = (y - 1) * 8 + ((9 - x) - 1) + 1
valid_xy((x, y)) = x >= 1 && x <= 8 && y >= 1 && y <= 8
xy_of_idx(idx::SquareIdx) = (9 - ((idx - 1) % 8 + 1), (idx - 1) ÷ 8 + 1)

mutable struct Piece
  type::PieceType
  color::Color
  dir::Direction
end

const WM = Piece(MONARCH, WHITE, S)
const WPL = Piece(PAWN, WHITE, NE)
const WPR = Piece(PAWN, WHITE, NW)

const BM = Piece(MONARCH, BLACK, N)
const BPL = Piece(PAWN, BLACK, SE)
const BPR = Piece(PAWN, BLACK, SW)

const NA = nothing

const Square = Union{Nothing, Piece}
const Board = Array{Square, 1}

struct NullMove end
struct TranslateMove
  from :: SquareIdx
  to :: SquareIdx
end
struct RotateMove 
  square :: SquareIdx
  rot :: Rotation
end

const Move = Union{NullMove, TranslateMove, RotateMove}
const TOTAL_MOVES = 1 + 420 + 3*64
const ActionMask = Array{Move, 1}

# helpers for our structures
function rotate_direction(dir::Direction, rot::Rotation)
  if dir == N
    (rot == LEFT && (return W))
    (rot == RIGHT && (return E))
    (rot == UTURN && (return S))
  elseif dir == E
    (rot == LEFT && (return N))
    (rot == RIGHT && (return S))
    (rot == UTURN && (return W))
  elseif dir == S
    (rot == LEFT && (return E))
    (rot == RIGHT && (return W))
    (rot == UTURN && (return N))
  elseif dir == W
    (rot == LEFT && (return S))
    (rot == RIGHT && (return N))
    (rot == UTURN && (return E))

  elseif dir == NE
    (rot == LEFT && (return NW))
    (rot == RIGHT && (return SE))
    (rot == UTURN && (return SW))
  elseif dir == NW
    (rot == LEFT && (return SW))
    (rot == RIGHT && (return NE))
    (rot == UTURN && (return SE))
  elseif dir == SE
    (rot == LEFT && (return NE))
    (rot == RIGHT && (return SW))
    (rot == UTURN && (return NW))
  elseif dir == SW
    (rot == LEFT && (return SE))
    (rot == RIGHT && (return NW))
    (rot == UTURN && (return NE))
  end
end

rotate_piece(p::Piece, rot::Rotation) = Piece(p.type, p.color, rotate_direction(p.dir, rot))

function qi_at(sq::SquareIdx)
  sq -= 1 # to account for julia arrays
  f = sq % 8
  r = div(sq, 8) 
  rows_from_center = 2 * r - 7
  files_from_center = 2 * f - 7
  return 98 - (rows_from_center * rows_from_center + files_from_center * files_from_center)
end

function square_attacks(sq)
  surrounding = [sq - 8, sq + 8]
  if sq % 8 != 1
      push!(surrounding, sq - 9)
      push!(surrounding, sq - 1)
      push!(surrounding, sq + 7)
  end
  if sq % 8 != 0
      push!(surrounding, sq - 7)
      push!(surrounding, sq + 1)
      push!(surrounding, sq + 9)
  end
  surrounding = filter(x -> x <= 64 && x >= 1, surrounding)
  return surrounding
end

function create_all_actions()
  actions = []
  push!(actions, NullMove())
  for from in 1:64
    new_moves = square_attacks(from)
    for to in new_moves
      push!(actions, TranslateMove(from, to))
    end
  end
  for sq in 1:64
    push!(actions, RotateMove(sq, RIGHT))
    push!(actions, RotateMove(sq, UTURN))
    push!(actions, RotateMove(sq, LEFT))
  end
  actions
end

const ACTIONS = create_all_actions()
const TOTAL_ACTIONS = size(ACTIONS)

# TODO: we could have the game parametrized by grid size.
struct GameSpec <: GI.AbstractGameSpec end

mutable struct GameEnv <: GI.AbstractGameEnv
  board :: Board
  current_player :: Color

  is_finished::Bool
  winner::Color
  moves_since_capture::Int32
  action_mask::Vector{Bool}
end

function generate_action_mask(board::Board, current_player::Color)
  actions = [true]
  for from in 1:64
    new_moves = square_attacks(from)
    is_our_piece = board[from] != NA && board[from].color == current_player
    for to in new_moves
      # qi is higher and theres not a king there
      has_more_chi = qi_at(Int8(from)) > qi_at(Int8(to)) && (board[to] == NA || board[to].type == PAWN)
      if is_our_piece && (board[to] == NA || has_more_chi)
        push!(actions, true)
      else
        push!(actions, false)
      end
    end
  end
  for sq in 1:64
    if board[sq] != NA && board[sq].color == current_player
      push!(actions, true)
      push!(actions, true)
      push!(actions, true)
    else
      push!(actions, false)
      push!(actions, false)
      push!(actions, false)
    end
  end
  actions
end

function GI.init(::GameSpec)
  # reverse so top left is 63 and bot right is 0
  board = reverse([
    BM,  NA,  NA, NA,  NA,  NA, NA,  BM  ,
    BPL, BPR, NA, BPL, BPR, NA, BPL, BPR ,
    NA,  NA,  NA, NA,  NA,  NA, NA,  NA  ,
    NA,  NA,  NA, NA,  NA,  NA, NA,  NA  ,
    NA,  NA,  NA, NA,  NA,  NA, NA,  NA  ,
    NA,  NA,  NA, NA,  NA,  NA, NA,  NA  ,
    WPL, WPR, NA, WPL, WPR, NA, WPL, WPR ,
    WM,  NA,  NA, NA,  NA,  NA, NA,  WM  ,
  ])
  return GameEnv(board, WHITE, false, WHITE, 0, generate_action_mask(board, WHITE))
end

GI.two_players(::GameSpec) = true

GI.spec(::GameEnv) = GameSpec()

#####
##### Game API
#####


GI.actions(::GameSpec) = ACTIONS
GI.actions_mask(g::GameEnv) = g.action_mask
GI.current_state(g::GameEnv) = deepcopy(g)

function GI.set_state!(g::GameEnv, state)
  g.action_mask = deepcopy(state.action_mask)
  g.board = deepcopy(state.board)
  g.current_player = deepcopy(state.current_player)
  g.is_finished = deepcopy(state.is_finished)
  g.moves_since_capture = deepcopy(state.moves_since_capture)
  g.winner = deepcopy(state.winner)
end

function get_push_square(from::SquareIdx, to::SquareIdx)
  from_xy = xy_of_idx(from)
  to_xy = xy_of_idx(to)
  dir_y = to_xy[2] - from_xy[2]
  dir_x = to_xy[1] - from_xy[1]
  @assert abs(dir_y) <= 1 && abs(dir_x) <= 1
  return (from_xy[2] + dir_y * 2, from_xy[2] + dir_x * 2)
end

function get_monarchs(board::Board, color::Color)
  monarchs = []
  i = 0
  for piece in board
    i += 1
    if piece != NA && piece.color == color && piece.type == MONARCH
      push!(monarchs, SquareIdx(i))
    end
  end
  monarchs
end

function move_xy_in_dir(x, y, dir::MonarchDirection)
  if dir == N
    return (x, y - 1)
  elseif dir == E
    return (x + 1, y)
  elseif dir == S
    return (x, y - 1)
  else
    return (x - 1, y)
  end
end

function pawn_bounce(pawn_dir::PawnDirection, dir::MonarchDirection)
  if pawn_dir == NE
    (dir == S && (return E))
    (dir == W && (return N))
  elseif pawn_dir == NW
    (dir == S && (return W))
    (dir == E && (return N))
  elseif pawn_dir == SE
    (dir == N && (return E))
    (dir == W && (return S))
  elseif pawn_dir == SW
    (dir == N && (return W))
    (dir == E && (return S))
  else
    return nothing
  end
end

function fire_laser(board::Board, monarch_sq::SquareIdx)
  x, y = xy_of_idx(monarch_sq)
  dir::MonarchDirection = board[monarch_sq].dir
  x, y = move_xy_in_dir(x, y, dir)
  while valid_xy((x, y))
    piece = board[idx_of_xy((x, y))]
    if piece != NA
      if piece.type == MONARCH
        return idx_of_xy((x, y))
      elseif piece.type == PAWN
        next_bounce = pawn_bounce(piece.dir, dir)
        if next_bounce === nothing
          return idx_of_xy((x, y))
        else
          dir = next_bounce
        end
      end
    end
    x, y = move_xy_in_dir(x, y, dir)
  end
  return nothing
end

function GI.play!(g::GameEnv, action)
  @assert !g.is_finished
  # change g.board, and update g.winner, g.is_finished
  # and g.moves_since_capture
  # checkmate resets capture timer too
  # stage 1: fire lasers
  if isa(action, TranslateMove)
    x, y = get_push_square(action.from, action.to)
    next = idx_of_xy((x, y))
    if valid_xy((x, y)) && g.board[next] == NA
      @assert 1 <= next <= 64
      g.board[next] = g.board[action.to]
    end
    g.board[action.to] = g.board[action.from]
    g.board[action.from] = NA
  elseif isa(action, RotateMove)
    g.board[action.square] = rotate_piece(g.board[action.square], action.rot)
  else
    # null move do nothing
  end
  # stage 2: fire lasers
  zapped = 0
  pre = deepcopy(g.board)
  monarchs = get_monarchs(g.board, g.current_player)
  zaps = []
  for monarch in monarchs
    if g.board[monarch] === nothing
      println("firing laser at: $monarch")
      println("Preboard:")
      println(pre)
      println("board")
      println(g.board)
      println("monarchs")
      println(monarchs)
    end
    square = fire_laser(g.board, monarch)
    if square !== nothing
      push!(zaps, square)
    end
  end
  for square in zaps
    g.board[square] = NA
    zapped += 1
  end

  curr_monarchs = get_monarchs(g.board, g.current_player)
  other_monarchs = get_monarchs(g.board, !g.current_player)
  if curr_monarchs < other_monarchs
    g.winner = !g.current_player
    g.is_finished = true
    g.moves_since_capture = 0
  end
  if other_monarchs == 0
    g.winner = g.current_player
    g.is_finished = true
    g.moves_since_capture = 0
  end
  if zapped == 0
    g.moves_since_capture += 1
  else
    g.moves_since_capture = 0
  end
  if g.moves_since_capture == 50
    g.is_finished = true
  end
  g.current_player = !g.current_player
  g.action_mask = generate_action_mask(g.board, g.current_player)
end

GI.white_playing(g::GameEnv) = g.current_player == WHITE
GI.game_terminated(g::GameEnv) = g.is_finished
function GI.white_reward(g::GameEnv)
  if g.is_finished
    if g.moves_since_capture == 50
      return 0
    end
    g.winner == WHITE && (return 1.)
    g.winner == BLACK && (return -1.)
  else
    return 0
  end
end

#####
##### Machine Learning API
#####

# Vectorized representation: 3x3x3 array
# Channels: free, white, black
# The board is represented from the perspective of white
# (as if white were to play next)

function flip_piece(p::Piece)
  return Piece(p.type, !p.color, p.dir)
end

function flip_colors(board::Board)
  list = [
    piece == NA ? NA : flip_piece(piece)
    for piece in board
  ]
  return Board(list...)
end

function generate_piece_types()
  pieces = []
  for color in [WHITE, BLACK]
      for dir in [N, W, S, E]
        push!(pieces, Piece(MONARCH, color, dir))
      end
  end
  for color in [WHITE, BLACK]
    for dir in [NE, NW, SW, SE]
      push!(pieces, Piece(PAWN, color, dir))
    end
  end
  pieces
end

const piece_types = generate_piece_types()

function GI.vectorize_state(::GameSpec, state::GameEnv)
  board = state.current_player == WHITE ? state.board : flip_colors(state.board)
  return Float32[
    board[idx_of_xy((col,row))] == c
    for col in 1:8,
        row in 1:8,
        c in piece_types]
end


