# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

  All_My_Pieces = [
    *All_Pieces,
    rotations([[0, 0], [1, 0], [0, 1], [1, 1], [0, 2]]),
    [
      [[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]],
      [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]
    ],
    rotations([[0, 0], [0, 1], [1, 0]])
  ]

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new([[[0,0]]], board)
  end
end

class MyBoard < Board

  attr_accessor :next_piece_is_cheat_piece, :score

  def initialize(game)
    super(game)
    @current_block = MyPiece.next_piece(self)
  end

  def next_piece
    super
    if next_piece_is_cheat_piece
      @current_block = MyPiece.cheat_piece(self)
      @game.processing_cheat = false
      @next_piece_is_cheat_piece = false
    else
      @current_block = MyPiece.next_piece(self)
    end
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    locations.each_index{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris

  attr_accessor :processing_cheat

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def do_cheat
    if @board.score >= 100 && ! @processing_cheat
      @board.next_piece_is_cheat_piece = true
      @processing_cheat = true
      @board.score -= 100
    end
  end

  def key_bindings
    @root.bind('u', proc { @board.rotate_counter_clockwise; @board.rotate_counter_clockwise })
    @root.bind('c', proc { do_cheat })
    super
  end
end
