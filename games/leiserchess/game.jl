import AlphaZero.GI
using StaticArrays
using Crayons
using Base

const NUM_COLS = 8
const NUM_ROWS = 8
const NUM_SQUARES = NUM_COLS * NUM_ROWS

const Color = Bool
const WHITE = true
const BLACK = false

const PieceType = Bool
const PAWN = true
const MONARCH = false

python_script_path = "./gameloop.py"
process = open(`python3 $(python_script_path)`, read=true, write=true)

# Function to send a command to the subprocess
function send_command(proc, cmd::String)
  println(proc, cmd)
end

# Function to read a response from the subprocess
function read_response(proc)
  return readline(proc)
end

# Function to parse the display output and update the board
function update_board_from_display(display_output::String)
  lines = split(display_output, '\n')
  board = Array{Square, 2}(undef, 8, 8)

  for (i, line) in enumerate(lines)
      if occursin("info", line)
          # Parse and update game state (like current player, etc.)
      elseif occursin(r"^\s*\d", line)  # Lines with board state
          row_data = split(strip(line[5:end]))  # Remove 'info <num>' part
          row = parse(Int, strip(line[5]))  # Parse row number

          for (col, piece_str) in enumerate(row_data)
              board[row + 1, col] = parse_square(piece_str)
          end
      end
  end

  return board
end

# Function to convert a string to a Square
function parse_square(str::String)
  if str == "--"
      return nothing
  else
      color = str[1] in ['N', 'S'] ? BLACK : WHITE
      type = str[2] == 'S' ? 'M' : 'P'  # 'M' for Monarch, 'P' for Pawn
      return Piece(type, color)
  end
end

function print_board(board)
  for row in 1:NUM_ROWS
    for col in 1:NUM_COLS
      piece = board[row, col]
      if piece == nothing
        print("-- ")
      else
        piece_str = (piece.color == WHITE ? "W" : "B") * (piece.type == PAWN ? "P" : "M")
        print(piece_str, " ")
      end
    end
    println()
  end
end

function GI.init(::GameSpec)
  # Send command to initialize the game to the starting position
  send_command(process, "position startpos")
  # Send command to display the current board state
  send_command(process, "display")
  # Read the engine's response
  display_output = read_response(process)

  # Parse the display output to get the initial board state
  initial_board = update_board_from_display(display_output)

  print_board(initial_board)

  # Determine the initial player (assuming White starts)
  initial_player = WHITE

  # Create and return the initial game environment
  return GameEnv(initial_board, initial_player, false, nothing, 0, [], [])
end


























# @enum(MonarchDirection, N, W, S, E)
# @enum(PawnDirection, NE, NW, SW, SE)
# const Direction = Union{MonarchDirection, PawnDirection}
# @enum(Rotation, RIGHT, UTURN, LEFT)
# const SquareIdx = Int8

# idx_of_xy((x, y)) = SquareIdx((y - 1) * 8 + ((9 - x) - 1) + 1)
# valid_xy((x, y)) = x >= 1 && x <= 8 && y >= 1 && y <= 8
# xy_of_idx(idx::SquareIdx) = (9 - ((idx - 1) % 8 + 1), (idx - 1) ÷ 8 + 1)

# mutable struct Piece
#   type::PieceType
#   color::Color
#   dir::Direction
# end

# Base.:(==)(a::Piece, b::Piece) = a.type == b.type && a.color == b.color && a.dir == b.dir

# const WM = Piece(MONARCH, WHITE, S)
# const WPL = Piece(PAWN, WHITE, NE)
# const WPR = Piece(PAWN, WHITE, NW)

# const BM = Piece(MONARCH, BLACK, N)
# const BPL = Piece(PAWN, BLACK, SE)
# const BPR = Piece(PAWN, BLACK, SW)

# const NA = nothing

# const Square = Union{Nothing, Piece}
# # const Board = Array{Square, 1}
# const Board = SMatrix{NUM_ROWS, NUM_COLS, Square, NUM_SQUARES}

# struct NullMove end
# struct TranslateMove
#   from :: SquareIdx
#   to :: SquareIdx
# end
# struct RotateMove 
#   square :: SquareIdx
#   rot :: Rotation
# end

# const Move = Union{NullMove, TranslateMove, RotateMove}
# const TOTAL_MOVES = 1 + 420 + 3*64
# const ActionMask = Array{Move, 1}

# function create_all_actions()
#   # actions = []
#   # push!(actions, NullMove())
#   # for row in 1:NUM_ROWS
#   #   for col in 1:NUM_COLS
#   #     from_sq = idx_of_xy((col, row))
#   #     new_moves = square_attacks(from_sq)
#   #     for to_sq in new_moves
#   #       push!(actions, TranslateMove(from_sq, to_sq))
#   #     end
#   #   end
#   # end
#   # for row in 1:NUM_ROWS
#   #   for col in 1:NUM_COLS
#   #     sq = idx_of_xy((col, row))
#   #     push!(actions, RotateMove(sq, RIGHT))
#   #     push!(actions, RotateMove(sq, UTURN))
#   #     push!(actions, RotateMove(sq, LEFT))
#   #   end
#   # end
#   # if size(actions)[1] != TOTAL_MOVES
#   #   len = size(actions)[1]
#   #   # println("total actions: $len")
#   # end
#   # @assert size(actions)[1] == TOTAL_MOVES
#   # actions
# end

# const ACTIONS = create_all_actions()
# const TOTAL_ACTIONS = size(ACTIONS)

# # TODO: we could have the game parametrized by grid size.
# struct GameSpec <: GI.AbstractGameSpec end

# mutable struct GameEnv <: GI.AbstractGameEnv
#   board :: Board
#   current_player :: Color

#   is_finished::Bool
#   winner::Color
#   moves_since_capture::Int32
#   action_mask::Vector{Bool}
#   move_log::Vector{String}
# end

# Base.:(==)(a::GameEnv, b::GameEnv) = a.board == b.board && a.current_player == b.current_player && a.is_finished == b.is_finished && a.winner == b.winner && a.moves_since_capture == b.moves_since_capture && a.action_mask == b.action_mask

# # function move_xy_in_dir(x, y, dir::MonarchDirection)
# #   # println("move_xy_in_dir: $x, $y, $dir")
# #   if dir == N
# #     return (x, y + 1)
# #   elseif dir == E
# #     return (x + 1, y)
# #   elseif dir == S
# #     return (x, y - 1)
# #   else
# #     return (x - 1, y)
# #   end
# # end

# # function fire_laser(board::Board, monarch_sq::SquareIdx)
# #   ox, oy = xy_of_idx(monarch_sq)
# #   dir::MonarchDirection = board[oy,ox].dir
# #   x, y = move_xy_in_dir(ox, oy, dir)
# #   while valid_xy((x, y))
# #     piece = board[y, x]
# #     if piece != NA
# #       if piece.type == MONARCH
# #         # println("monarch hit at: $x, $y, from $ox, $oy")
# #         return idx_of_xy((x, y))
# #       elseif piece.type == PAWN
# #         # print piece.dir and dir
# #         # println("x,y: $(x), $(y)")
# #         # println("piece.dir: $(piece.dir)")
# #         # println("dir: $(dir)")
# #         next_bounce = pawn_bounce(piece.dir, dir)
# #         if next_bounce === nothing
# #           # println("returning")
# #           return idx_of_xy((x, y))
# #         else
# #           dir = next_bounce
# #         end
# #       end
# #     end
# #     x, y = move_xy_in_dir(x, y, dir)
# #   end
# #   return nothing
# # end

# function generate_action_mask(board::Board, current_player::Color)
#   actions = [false]
#   for row in 1:NUM_ROWS
#     for col in 1:NUM_COLS
#       # Check for Monarch and laser firing possibility
#       if board[row, col] != NA && board[row, col].type == MONARCH && board[row, col].color == current_player
#         # println("Checking Monarch at: ($col, $row)")
#         if fire_laser(board, Int8(idx_of_xy((col, row)))) !== nothing
#           actions[1] = true
#           # println("Laser can be fired from: ($col, $row)")
#           break
#         end
#       end
#     end
#   end
#   for row in 1:NUM_ROWS
#     for col in 1:NUM_COLS
#         from_sq = idx_of_xy((col, row))
#         new_moves = square_attacks(from_sq)
#         moving_piece = board[row, col]
#         is_our_piece = moving_piece != NA && moving_piece.color == current_player

#         for to_sq in new_moves
#             (to_col, to_row) = xy_of_idx(Int8(to_sq))
#             target_piece = board[to_row, to_col]

#             can_shove = target_piece == NA
#             if moving_piece != NA && moving_piece.type == MONARCH
#                 can_shove = can_shove || target_piece == NA || target_piece.type == PAWN
#             elseif moving_piece != NA && moving_piece.type == PAWN && target_piece != NA && target_piece.type == PAWN
#                 can_shove = can_shove || qi_at(Int8(from_sq)) >= qi_at(Int8(to_sq))
#             end

#             if is_our_piece && can_shove
#                 # println("Valid move from: ($col, $row) to: ($to_col, $to_row)")
#                 push!(actions, true)
#             else
#                 # println("Invalid move from: ($col, $row) to: ($to_col, $to_row)")
#                 push!(actions, false)
#             end
#         end
#     end
#   end
#   for row in 1:NUM_ROWS
#     for col in 1:NUM_COLS
#         if board[row, col] != NA && board[row, col].color == current_player
#             # println("Adding rotation moves for piece at: ($col, $row)")
#             push!(actions, true)  # Rotate RIGHT
#             push!(actions, true)  # Rotate UTURN
#             push!(actions, true)  # Rotate LEFT
#         else
#             push!(actions, false)
#             push!(actions, false)
#             push!(actions, false)
#         end
#     end
#   end
#   @assert length(actions) == TOTAL_MOVES
#   # println("actions: $actions")
#   deepcopy(actions)
# end

# # reverse so top left is 63 and bot right is 0
# initial_board_array = reverse([
#   BM,  NA,  NA, NA,  NA,  NA, NA,  BM  ,
#   BPR, BPL, NA, BPR, BPL, NA, BPR, BPL ,
#   NA,  NA,  NA, NA,  NA,  NA, NA,  NA  ,
#   NA,  NA,  NA, NA,  NA,  NA, NA,  NA  ,
#   NA,  NA,  NA, NA,  NA,  NA, NA,  NA  ,
#   NA,  NA,  NA, NA,  NA,  NA, NA,  NA  ,
#   WPR, WPL, NA, WPR, WPL, NA, WPR, WPL ,
#   WM,  NA,  NA, NA,  NA,  NA, NA,  WM  ,
# ])

# function transpose_board(board::Board)
#   transposed_board = similar(board)
#   for row in 1:NUM_ROWS
#       for col in 1:NUM_COLS
#           transposed_board[col, row] = board[row, col]
#       end
#   end
#   return SMatrix{NUM_ROWS, NUM_COLS, Square, NUM_ROWS * NUM_COLS}(transposed_board)
# end

# const INITIAL_BOARD = transpose_board(SMatrix{NUM_ROWS, NUM_COLS, Square, NUM_ROWS * NUM_COLS}(reshape(initial_board_array, NUM_ROWS, NUM_COLS)))

# const INITIAL_MOVES = generate_action_mask(INITIAL_BOARD, WHITE)

# const INITIAL_STATE = GameEnv(INITIAL_BOARD, WHITE, false, WHITE, 0, INITIAL_MOVES, [])

# GI.two_players(::GameSpec) = true

# GI.spec(::GameEnv) = GameSpec()

# #####
# ##### Game API
# #####

# GI.actions(::GameSpec) = deepcopy(ACTIONS)

# GI.actions_mask(g::GameEnv) = deepcopy(g.action_mask)
# GI.current_state(g::GameEnv) = (
#     board = g.board, 
#     curplayer = g.current_player, 
#     is_finished = g.is_finished, 
#     winner = g.winner, 
#     moves_since_capture = g.moves_since_capture
# )

# function GI.set_state!(g::GameEnv, state)
#   g.board = state.board
#   g.current_player = state.curplayer
#   g.is_finished = state.is_finished
#   g.winner = state.winner
#   g.moves_since_capture = state.moves_since_capture
#   g.action_mask = generate_action_mask(g.board, g.current_player)
# end

# function get_push_square(from::SquareIdx, to::SquareIdx)
#   from_xy = xy_of_idx(from)
#   to_xy = xy_of_idx(to)
#   dir_y = to_xy[2] - from_xy[2]
#   dir_x = to_xy[1] - from_xy[1]
#   @assert abs(dir_y) <= 1 && abs(dir_x) <= 1
#   return (from_xy[2] + dir_y * 2, from_xy[2] + dir_x * 2)
# end

# function get_monarchs(board::Board, color::Color)
#   monarchs = SquareIdx[]
#   for row in 1:NUM_ROWS
#     for col in 1:NUM_COLS
#       if board[row, col] != NA && board[row, col].color == color && board[row, col].type == MONARCH
#         # println("found monarch at: $row, $col")
#         push!(monarchs, SquareIdx(idx_of_xy((col, row))))
#       end
#     end
#   end
#   monarchs
# end

# function pawn_bounce(pawn_dir::PawnDirection, dir::MonarchDirection)
#   if pawn_dir == NE
#     (dir == S && (return E))
#     (dir == W && (return N))
#   elseif pawn_dir == NW
#     (dir == S && (return W))
#     (dir == E && (return N))
#   elseif pawn_dir == SE
#     (dir == N && (return E))
#     (dir == W && (return S))
#   elseif pawn_dir == SW
#     (dir == N && (return W))
#     (dir == E && (return S))
#   end
#   # println("returning nothing")
#   return nothing
# end

# function format_move(action)
#   if isa(action, TranslateMove)
#       from_xy = xy_of_idx(action.from)
#       to_xy = xy_of_idx(action.to)
#       return "($(from_xy[1]),$(from_xy[2])) -> ($(to_xy[1]),$(to_xy[2]))"
#   elseif isa(action, RotateMove)
#       square_xy = xy_of_idx(action.square)
#       rot_str = action.rot == RIGHT ? "R" : action.rot == UTURN ? "U" : "L"
#       return "($(square_xy[1]),$(square_xy[2])) rot $rot_str"
#   end
#   return "Invalid action"
# end

# function GI.play!(g::GameEnv, action)
#   # println("play!")
#   @assert !g.is_finished
#   # println("not finished")
#   # change g.board, and update g.winner, g.is_finished
#   # and g.moves_since_capture
#   # checkmate resets capture timer too
#   # render(g.board)
#   # println("player: $(g.current_player)")
#   # println("action: $action")

#   # Find the index of the action in the list of all possible actions
#   action_index = findfirst(==(action), ACTIONS)
#   # println("action index: $action_index")
#   # println("g.action_mask: $(g.action_mask)")

#   # Check if the action is in the action mask
#   if action_index === nothing || !g.action_mask[action_index]
#     # println("Invalid action: $action is not in the action mask.")
#     return
#   end

#   formatted_action = format_move(action)
#   # GI.render(g)
#   # println(formatted_action)
#   push!(g.move_log, formatted_action)

#   # stage 1: fire lasers
#   mutable_board = Array(deepcopy(g.board))
#   if isa(action, TranslateMove)
#     from_xy = xy_of_idx(action.from)
#     to_xy = xy_of_idx(action.to)

#     moving_piece = mutable_board[from_xy[2], from_xy[1]]
#     shove_direction = (to_xy[1] - from_xy[1], to_xy[2] - from_xy[2])
#     current_xy = to_xy
#     moving_qi = qi_at(action.from)

#     # Loop through squares in the shove direction
#     while true
#       next_xy = (current_xy[1] + shove_direction[1], current_xy[2] + shove_direction[2])

#       # Check if next square is within bounds
#       if !valid_xy(next_xy)
#           break
#       end

#       target_piece = mutable_board[next_xy[2], next_xy[1]]

#       # If the next square is empty, stop the shove process
#       if target_piece == NA
#           break
#       end

#       target_qi = qi_at(idx_of_xy((next_xy[1], next_xy[2])))

#       # Check for shoving logic based on piece types and qi
#       if moving_piece.type == PAWN && target_piece.type == MONARCH
#           # Pawn cannot shove a monarch
#           break
#       elseif moving_piece.type == MONARCH && target_piece.type == PAWN
#           # Monarch can shove a pawn
#           mutable_board[next_xy[2], next_xy[1]] = mutable_board[current_xy[2], current_xy[1]]
#           mutable_board[current_xy[2], current_xy[1]] = NA
#           current_xy = next_xy
#       elseif moving_qi >= target_qi
#           # General shove logic based on qi
#           mutable_board[next_xy[2], next_xy[1]] = NA
#           current_xy = next_xy
#       else
#           break
#       end
#     end

#     # Update the board with the moving piece
#     mutable_board[to_xy[2], to_xy[1]] = moving_piece
#     mutable_board[from_xy[2], from_xy[1]] = NA
#   elseif isa(action, RotateMove)
#     # Convert the linear index to (row, col)
#     square_x, square_y = xy_of_idx(action.square)
#     if g.board[square_y, square_x] != NA
#       mutable_board[square_y, square_x] = rotate_piece(g.board[square_y, square_x], action.rot)  # Rotate the piece
#     end
#   else
#     # null move do nothing
#   end
#   g.board = SMatrix{NUM_ROWS, NUM_COLS, Square, NUM_ROWS * NUM_COLS}(reshape(mutable_board, NUM_ROWS, NUM_COLS))

#   # render(g.board)
#   # stage 2: fire lasers
#   zapped = 0
#   pre = deepcopy(mutable_board)
#   monarchs = get_monarchs(g.board, g.current_player)
#   zaps = []
#   for monarch in monarchs
#     x, y = xy_of_idx(monarch)
#     # if g.board[y, x] === nothing
#     #   println("firing laser at: $monarch")
#     #   println("Preboard:")
#     #   println(pre)
#     #   println("board")
#     #   println(g.board)
#     #   println("monarchs")
#     #   println(monarchs)
#     # end
#     square = fire_laser(g.board, monarch)
#     if square !== nothing
#       push!(zaps, square)
#     end
#   end
#   mutable_board = Array(deepcopy(g.board))
#   for square in zaps
#     x,y = xy_of_idx(Int8(square))
#     mutable_board[y,x] = NA
#     zapped += 1
#   end
#   g.board = SMatrix{NUM_ROWS, NUM_COLS, Square, NUM_ROWS * NUM_COLS}(mutable_board)

#   curr_monarchs = get_monarchs(g.board, g.current_player)
#   other_monarchs = get_monarchs(g.board, !g.current_player)
#   if length(curr_monarchs) < length(other_monarchs)
#     g.winner = !g.current_player
#     g.is_finished = true
#     g.moves_since_capture = 0
#   end
#   if length(other_monarchs) == 0
#     g.winner = g.current_player
#     g.is_finished = true
#     g.moves_since_capture = 0
#   end
#   if zapped == 0
#     g.moves_since_capture += 1
#   else
#     g.moves_since_capture = 0
#   end
#   if g.moves_since_capture == 50
#     g.is_finished = true
#   end

#   # if g.is_finished
#   #   println("Winner: $(g.winner)")
#   #   println("GAME OVER!!")
#   #   GI.render(g)
#   #   println("\nMove Log:")
#   #   for (i, move) in enumerate(g.move_log)
#   #     println("Move $i: $move")
#   #   end
#   # end
  
#   # println("finished move and generating action mask")

#   g.current_player = !g.current_player
#   g.action_mask = generate_action_mask(g.board, g.current_player)
# end

# GI.white_playing(g::GameEnv) = g.current_player == WHITE
# GI.game_terminated(g::GameEnv) = g.is_finished
# function GI.white_reward(g::GameEnv) :: Float64
#   if g.is_finished
#     # println("game finish: $(g.winner)")
#     if g.moves_since_capture == 50
#       return 0.0
#     elseif g.winner == WHITE
#       return 1.0
#     elseif g.winner == BLACK
#       return -1.0
#     end
#   end
#   return 0.0
# end

# #####
# ##### Machine Learning API
# #####

# # Vectorized representation: 3x3x3 array
# # Channels: free, white, black
# # The board is represented from the perspective of white
# # (as if white were to play next)

# function flip_piece(p::Piece)
#   return Piece(p.type, !p.color, p.dir)
# end

# function flip_colors(board::Board)
#   new_board = [piece == NA ? NA : flip_piece(piece) for piece in board]
#   return SMatrix{NUM_ROWS, NUM_COLS, Square, NUM_ROWS * NUM_COLS}(new_board)
# end

# function generate_piece_types()
#   pieces = []
#   for color in [WHITE, BLACK]
#       for dir in [N, W, S, E]
#         push!(pieces, Piece(MONARCH, color, dir))
#       end
#   end
#   for color in [WHITE, BLACK]
#     for dir in [NE, NW, SW, SE]
#       push!(pieces, Piece(PAWN, color, dir))
#     end
#   end
#   pieces
# end

# const piece_types = generate_piece_types()

# function GI.vectorize_state(::GameSpec, state)
#   board = state.curplayer == WHITE ? state.board : flip_colors(state.board)
#   return Float32[
#     board[row, col] == c
#     for row in 1:NUM_ROWS, col in 1:NUM_COLS, c in piece_types
#   ]
# end

# function GI.heuristic_value(g::GameEnv)
#   total = 0
#   for square in g.board
#     if square != NA && square.color == g.current_player
#       total += 1
#     end
#   end
#   return Float64(total)
# end

# #####
# ##### Symmetries
# #####

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

#     # println("parsing")

#     # Match for move action
#     move_match = match(move_pattern, str)
#     if move_match !== nothing
#         from_x, from_y = parse(Int, move_match.captures[1]), parse(Int, move_match.captures[2])
#         to_x, to_y = parse(Int, move_match.captures[3]), parse(Int, move_match.captures[4])
#         from_idx = idx_of_xy((from_x, from_y))
#         to_idx = idx_of_xy((to_x, to_y))
#         # println("translating")
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
#     # println("ACTIONS: $(ACTIONS[297])")
#     pname = player_name(g.current_player)
#     pcol = player_color(g.current_player)
#     print(pcol, pname, " plays:", crayon"reset", "\n\n")
#     # Print legend
#     for col in 1:NUM_COLS
#         print(GI.action_string(GI.spec(g), col), " ")
#     end
#     print("\n")
#     # Print board
#     for row in 1:NUM_ROWS
#         for col in 1:NUM_COLS
#           c = g.board[idx_of_xy((row, col))]
#             if c !== NA  # Add this check
#               color_str = c.color == WHITE ? "w" : "b"
#               symbol = cell_mark(c)
#               print(symbol, color_str, " ")
#             else
#               print(". ")  # Handle the nothing case
#             end
#         end
#         println()  # New line after each row
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