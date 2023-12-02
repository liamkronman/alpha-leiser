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

Base.:(==)(a::Piece, b::Piece) = a.type == b.type && a.color == b.color && a.dir == b.dir

const WM = Piece(MONARCH, WHITE, S)
const WPL = Piece(PAWN, WHITE, NE)
const WPR = Piece(PAWN, WHITE, NW)

const BM = Piece(MONARCH, BLACK, N)
const BPL = Piece(PAWN, BLACK, SE)
const BPR = Piece(PAWN, BLACK, SW)

const NA = nothing

const Square = Union{Nothing, Piece}
# const Board = Array{Square, 1}
const Board = SMatrix{NUM_ROWS, NUM_COLS, Square, NUM_SQUARES}

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
  if size(actions)[1] != TOTAL_MOVES
    len = size(actions)[1]
    println("total actions: $len")
  end
  @assert size(actions)[1] == TOTAL_MOVES
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

Base.:(==)(a::GameEnv, b::GameEnv) = a.board == b.board && a.current_player == b.current_player && a.is_finished == b.is_finished && a.winner == b.winner && a.moves_since_capture == b.moves_since_capture && a.action_mask == b.action_mask

function generate_action_mask(board::Board, current_player::Color)
  actions = [false]
  for row in 1:NUM_ROWS
      for col in 1:NUM_COLS
          from_sq = idx_of_xy((col, row))
          new_moves = square_attacks(from_sq)
          is_our_piece = board[row, col] != NA && board[row, col].color == current_player
          for to_sq in new_moves
              (to_col, to_row) = xy_of_idx(Int8(to_sq))
              has_more_chi = qi_at(Int8(from_sq)) > qi_at(Int8(to_sq)) && (board[to_row, to_col] == NA || board[to_row, to_col].type == PAWN)
              if is_our_piece && (board[to_row, to_col] == NA || has_more_chi)
                  push!(actions, true)
              else
                  push!(actions, false)
              end
          end
      end
  end
  for row in 1:NUM_ROWS
      for col in 1:NUM_COLS
          if board[row, col] != NA && board[row, col].color == current_player
              push!(actions, true)  # Rotate RIGHT
              push!(actions, true)  # Rotate UTURN
              push!(actions, true)  # Rotate LEFT
          else
              push!(actions, false)
              push!(actions, false)
              push!(actions, false)
          end
      end
  end
  @assert length(actions) == TOTAL_MOVES
  actions
end

# reverse so top left is 63 and bot right is 0
initial_board_array = [
  WM,  NA,  NA, NA,  NA,  NA, NA,  WM,
  WPL, WPR, NA, WPL, WPR, NA, WPL, WPR,
  NA,  NA,  NA, NA,  NA,  NA, NA,  NA,
  NA,  NA,  NA, NA,  NA,  NA, NA,  NA,
  NA,  NA,  NA, NA,  NA,  NA, NA,  NA,
  NA,  NA,  NA, NA,  NA,  NA, NA,  NA,
  BPL, BPR, NA, BPL, BPR, NA, BPL, BPR,
  BM,  NA,  NA, NA,  NA,  NA, NA,  BM
]

const INITIAL_BOARD = SMatrix{NUM_ROWS, NUM_COLS, Square, NUM_ROWS * NUM_COLS}(reshape(initial_board_array, NUM_ROWS, NUM_COLS))

const INITIAL_MOVES = generate_action_mask(INITIAL_BOARD, WHITE)

const INITIAL_STATE = GameEnv(INITIAL_BOARD, WHITE, false, WHITE, 0, INITIAL_MOVES)

function GI.init(::GameSpec)
  return deepcopy(INITIAL_STATE)
end

GI.two_players(::GameSpec) = true

GI.spec(::GameEnv) = GameSpec()

#####
##### Game API
#####

GI.actions(::GameSpec) = deepcopy(ACTIONS)
GI.actions_mask(g::GameEnv) = deepcopy(g.action_mask)
GI.current_state(g::GameEnv) = (
    board = g.board, 
    curplayer = g.current_player, 
    is_finished = g.is_finished, 
    winner = g.winner, 
    moves_since_capture = g.moves_since_capture, 
    action_mask = g.action_mask
)

function GI.set_state!(g::GameEnv, state)
  g.board = state.board
  g.current_player = state.curplayer
  g.is_finished = state.is_finished
  g.winner = state.winner
  g.moves_since_capture = state.moves_since_capture
  g.amask = state.action_mask
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
  monarchs = SquareIdx[]
  for row in 1:NUM_ROWS
    for col in 1:NUM_COLS
      if board[row, col] != NA && board[row, col].color == color && board[row, col].type == MONARCH
        push!(monarchs, SquareIdx(idx_of_xy((col, row))))
      end
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
  x, y = xy_of_idx(Int8(monarch_sq))
  dir::MonarchDirection = board[monarch_sq].dir
  x, y = move_xy_in_dir(x, y, dir)
  while valid_xy((x, y))
    piece = board[x, y]
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
  mutable_board = Array(deepcopy(g.board))
  if isa(action, TranslateMove)
    # Convert the linear indices to (row, col)
    from_xy = xy_of_idx(action.from)
    to_xy = xy_of_idx(action.to)

    mutable_board[to_xy...] = g.board[from_xy...]  # Move the piece
    mutable_board[from_xy...] = NA  # Set the original position to NA
  elseif isa(action, RotateMove)
    # Convert the linear index to (row, col)
    square_xy = xy_of_idx(action.square)
    if g.board[square_xy...] != NA
      mutable_board[square_xy...] = rotate_piece(g.board[square_xy...], action.rot)  # Rotate the piece
    end
  else
    # null move do nothing
  end
  g.board = SMatrix{NUM_ROWS, NUM_COLS, Square, NUM_ROWS * NUM_COLS}(reshape(mutable_board, NUM_ROWS, NUM_COLS))

  # stage 2: fire lasers
  zapped = 0
  pre = deepcopy(mutable_board)
  monarchs = get_monarchs(g.board, g.current_player)
  zaps = []
  for monarch in monarchs
    if mutable_board[monarch] === nothing
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
  mutable_board = Array(deepcopy(g.board))
  for square in zaps
    mutable_board[xy_of_idx(Int8(square))...] = NA
    zapped += 1
  end
  g.board = SMatrix{NUM_ROWS, NUM_COLS, Square, NUM_ROWS * NUM_COLS}(mutable_board)

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
  new_board = [piece == NA ? NA : flip_piece(piece) for piece in board]
  return SMatrix{NUM_ROWS, NUM_COLS, Square, NUM_ROWS * NUM_COLS}(new_board)
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

function GI.vectorize_state(::GameSpec, state)
  board = state.curplayer == WHITE ? state.board : flip_colors(state.board)
  return Float32[
    board[row, col] == c
    for row in 1:NUM_ROWS, col in 1:NUM_COLS, c in piece_types
  ]
end

function GI.heuristic_value(g::GameEnv)
  total = 0
  for square in g.board
    if square != NA && square.color == g.current_player
      total += 1
    end
  end
  return Float64(total)
end

#####
##### Symmetries
#####

# function flipped_board(board::Board)
#     # Create a flipped version of the board
#     flipped_board = copy(board)
#     for row in 1:NUM_ROWS
#         for col in 1:NUM_COLS ÷ 2
#             # Swap elements symmetrically across the middle column
#             opposite_col = NUM_COLS - col + 1
#             flipped_board[idx_of_xy((col, row))], flipped_board[idx_of_xy((opposite_col, row))] = flipped_board[idx_of_xy((opposite_col, row))], flipped_board[idx_of_xy((col, row))]
#         end
#     end
#     return flipped_board
# end

# function GI.symmetries(::GameSpec, state::GameEnv)
#     flipped = flipped_board(state.board)
#     # Create a symmetrical game state
#     sym_state = GameEnv(flipped, state.current_player, state.is_finished, state.winner, state.moves_since_capture, generate_action_mask(flipped, state.current_player))
#     # Return the symmetrical state with the corresponding column transformation
#     return [(sym_state, reverse(1:NUM_COLS))]
# end

# #####
# ##### User interface
# #####

# GI.action_string(::GameSpec, a) = string(a)

# # format: "(x,y) -> (x+1,y)" or "(x,y) rot R" or "(x,y) rot U"
# function GI.parse_action(::GameSpec, str::String)
#     # Regex patterns to match different action types
#     move_pattern = r"\((\d+),(\d+)\) -> \((\d+),(\d+)\)"
#     rotate_pattern = r"\((\d+),(\d+)\) rot (R|U|L)"

#     # Match for move action
#     move_match = match(move_pattern, str)
#     if move_match !== nothing
#         from_x, from_y = parse(Int, move_match.captures[1]), parse(Int, move_match.captures[2])
#         to_x, to_y = parse(Int, move_match.captures[3]), parse(Int, move_match.captures[4])
#         from_idx = idx_of_xy((from_x, from_y))
#         to_idx = idx_of_xy((to_x, to_y))
#         return TranslateMove(from_idx, to_idx)
#     end

#     # Match for rotate action
#     rotate_match = match(rotate_pattern, str)
#     if rotate_match !== nothing
#         x, y = parse(Int, rotate_match.captures[1]), parse(Int, rotate_match.captures[2])
#         rot_dir = rotate_match.captures[3]
#         square_idx = idx_of_xy((x, y))
#         rot = (rot_dir == "R" ? RIGHT : rot_dir == "U" ? UTURN : LEFT)
#         return RotateMove(square_idx, rot)
#     end

#     # If no matches, return nothing
#     return nothing
# end

# player_color(p) = p == WHITE ? crayon"light_red" : crayon"light_blue"
# player_name(p)  = p == WHITE ? "Red" : "Blue"

# function GI.render(g::GameEnv; with_position_names=true, botmargin=true)
#     pname = player_name(g.current_player)
#     pcol = player_color(g.current_player)
#     print(pcol, pname, " plays:", crayon"reset", "\n\n")
#     # Print legend
#     for col in 1:NUM_COLS
#         print(GI.action_string(GI.spec(g), col), " ")
#     end
#     print("\n")
#     # Print board
#     for row in NUM_ROWS:-1:1
#         for col in 1:NUM_COLS
#             c = g.board[idx_of_xy((col, row))]
#             print(cell_color(c), cell_mark(c), crayon"reset", " ")
#         end
#         print("\n")
#     end
#     botmargin && print("\n")
# end

# function cell_mark(square)
#     if square === NA
#         return "."
#     else
#         if square.type == MONARCH
#             return monarch_mark(square.dir)
#         elseif square.type == PAWN
#             return pawn_mark(square.dir)
#         end
#     end
# end

# function monarch_mark(dir)
#     # Unicode arrows for different directions
#     return dir == N ? "↑" : dir == E ? "→" : dir == S ? "↓" : "←"
# end

# function pawn_mark(dir)
#     # Unicode/ASCII triangles for different directions
#     return dir == NE ? "◣" : dir == NW ? "◢" : dir == SW ? "◥" : "◤"
# end

# function GI.read_state(::GameSpec)
#     # Initialize an empty board
#     board = Vector{Square}(undef, NUM_SQUARES)

#     try
#         # Read each row of the board
#         for row in 1:NUM_ROWS
#             input = readline()
#             # Process each character in the row
#             for (col, c) in enumerate(input)
#                 idx = idx_of_xy((col, row))
#                 board[idx] = char_to_piece(c)
#             end
#         end

#         # Determine the current player based on the count of pieces
#         nw, nb = count_pieces(board)
#         curplayer = determine_current_player(nw, nb)

#         if curplayer === nothing
#             return nothing
#         end

#         return GameEnv(board, curplayer, false, WHITE, 0, generate_action_mask(board, curplayer))
#     catch e
#         return nothing
#     end
# end

# function char_to_piece(c::Char)
#     c = lowercase(c)
#     piece_map = Dict(
#         '↑' => Piece(MONARCH, N), '→' => Piece(MONARCH, E),
#         '↓' => Piece(MONARCH, S), '←' => Piece(MONARCH, W),
#         '◣' => Piece(PAWN, NE), '◢' => Piece(PAWN, NW),
#         '◥' => Piece(PAWN, SW), '◤' => Piece(PAWN, SE),
#         '.' => NA
#     )
#     return get(piece_map, c, NA)
# end

# function count_pieces(board::Vector{Square})
#     nw, nb = 0, 0
#     for piece in board
#         if piece !== NA
#             piece.color == WHITE ? nw += 1 : nb += 1
#         end
#     end
#     return nw, nb
# end

# function determine_current_player(nw::Int, nb::Int)
#     if nw == nb
#         return WHITE
#     elseif nw == nb + 1
#         return BLACK
#     else
#         return nothing
#     end
# end