module LeiserChess
  export GameSpec
  include("game.jl")
  module Training
    using AlphaZero
    import ..GameSpec
    include("params.jl")
 end
end