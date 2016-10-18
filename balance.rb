require 'pry'

class Turtle
  attr_accessor :square, :color, :tokens, :turn_losses
  def initialize(color:)
    @tokens = 0
    @turn_losses = 0
    @color = color
  end

  def can_play?
    if @turn_losses > 0
      @turn_losses -= 1
      false
    else
      true
    end
  end
end

class Square
  attr_accessor :next, :num
  def initialize
    $num ||= 0
    $num += 1
    @num = $num
    @next = []
  end
  def process_player(player)
  end
end

class TokenSquare < Square
  attr_accessor :count
  def initialize(count:)
    super()
    @count = count
  end
  def process_player(player)
    # puts "player #{player.color} +#{@count} tokens"
    player.tokens += @count
  end
end
class ShortCutSquare < Square
  attr_accessor :cost, :savings
  def initialize(cost:, savings:)
    super()
    @cost = cost
    @savings = savings
  end
  def process_player(player)
    puts "player #{player.color} takes shortcut: -#{@cost} tokens"
    player.tokens -= @cost
  end
end

class BurglarSquare < Square
  def initialize(board:)
    super()
    @board = board
  end
  def process_player(player)
    roll = Die.roll
    others = (@board.players - [player])
    other = nil
    rich_players = others.select{|pl|pl.tokens >= roll}
    if rich_players.size > 0
      other = rich_players.sample
    else
      other = others.sort_by(&:tokens).last
      roll = other.tokens
    end

    player.tokens += roll
    other.tokens -= roll
    puts "player #{player.color} STOLE #{roll} tokens from #{other.color}"
  end
end

class LoseATurnSquare < Square
  def process_player(player)
    player.turn_losses += 1
  end
end

class Board
  attr_accessor :start, :finish, :players

  # TODO shortcut / price
  def self.generate(size:, players:, token_perc:, token_range:, lose_a_turn_perc:, shortcut_info:, shortcut_cost:)
    Board.new.tap do |b|
      b.players = players
      s = Square.new
      b.start = s
      shortcut = nil
      size.times do |i|

        if i == shortcut_info[0].min
          shortcut = ShortCutSquare.new(cost: shortcut_cost, savings: (shortcut_info[0].max - shortcut_info[0].min) - shortcut_info[1])
          s.next << shortcut
        end
        s.next << random_square(b, token_perc, token_range, lose_a_turn_perc)

        if s.is_a? ShortCutSquare
          shortcut_info[1].times do
            shortcut.next << random_square(b, token_perc, token_range, lose_a_turn_perc)
            shortcut = shortcut.next.last
          end
        end

        if i == shortcut_info[0].max
          shortcut.next << s
          shortcut = nil
        end
        s = s.next.first
      end

      s.next = [b.start]
      b.finish = s
      # b.print
    end
  end

  def self.random_square(board, token_perc, token_range, lose_a_turn_perc)
    if(rand(0..100) < token_perc)
      count = token_range.is_a?(Range) ? rand(token_range) : token_range.sample
      TokenSquare.new(count:count)
    else
      if(rand(0..100) < lose_a_turn_perc)
        LoseATurnSquare.new
      else

        if(rand(0..100) < 10)
          BurglarSquare.new(board: board)
        else
          Square.new
        end
      end
    end
  end

  def print
    tokens = []

    s = start
    board_size = 0
    burglars = 0
    turns = 0
    main_squares = ""
    mains = []

    while s != finish
      board_size += 1
      if s.is_a? TokenSquare
        main_squares << "#{s.count}"
        tokens << s.count
      elsif s.is_a? BurglarSquare
        main_squares << "$"
        burglars += 1
      elsif s.is_a? LoseATurnSquare
        main_squares << "X"
        turns += 1
      elsif s.is_a? ShortCutSquare
        main_squares << "S"
      else
        main_squares << "_"
      end
      mains << s
      s = s.next.first
    end

    s = start
    seen_shortcut_start = false
    seen_shortcut_finish = false

    shortcut_squares = ""
    # while !seen_shortcut_start || !mains.include?(s)
    savings = 0
    while s != finish
      board_size += 1
      if seen_shortcut_start && !seen_shortcut_finish
        if mains.include?(s)
          seen_shortcut_finish = true
        end

        if s.is_a? TokenSquare
          shortcut_squares << s.count
          tokens << s.count
          shortcut_squares << "#{s.count}"
        elsif s.is_a? BurglarSquare
          shortcut_squares << "$"
          burglars += 1
        elsif s.is_a? LoseATurnSquare
          shortcut_squares << "X"
          turns += 1
        else
          shortcut_squares << "_"
        end

      elsif !seen_shortcut_start
        shortcut_squares << " " unless s.is_a? ShortCutSquare
      else
        shortcut_squares << "." if savings > 0
        savings -= 1
      end

      if s.is_a? ShortCutSquare
        savings = s.savings
        seen_shortcut_start = true
        s = s.next[1]
      else
        s = s.next.first 
      end
    end


    puts main_squares
    puts shortcut_squares
    token_info = tokens.uniq.map.with_index{|t,i|"#{i+1}(#{tokens.count(t)})"}.join(",")
    puts "\n#{tokens.size}/#{board_size} token squares #{token_info}; #{turns} lose-a-turns; #{burglars} burglar squares"
  end

end

class Game
  attr_accessor :board, :players, :winner
  def initialize(board:, players:)
    @board = board
    @players = players
    @players.each do |pl|
      pl.square = board.start
    end
    @stats = {}
  end

  def play_to_completion!
    until @winner = play_next_player
      # noop
    end

    puts "\n#{@winner.color} won with #{@winner.tokens} token(s)"

    @stats
  end

  def play_next_player
    @turn ||= 0
    player = @players[@turn]
    @turn = (@turn + 1) % (@players.size)

    if player.can_play?
      roll = Die.roll
      puts "rolled: #{roll} for #{player.color}"

      roll.times do
        advance_player player
      end
    else
      puts "skipped: #{player.color}"
    end

    tokens_required_to_win = 20
    puts "#{player.color} LAP ===================" if player.square == @board.finish && player.tokens < tokens_required_to_win
    if player.square == @board.finish && player.tokens >= tokens_required_to_win
      player
    else
      nil
    end
  end

  def advance_player(player)
    if !player.square.is_a?(ShortCutSquare) #.next.size == 1
      player.square = player.square.next.first
      player.square.process_player player
    else
      cost = player.square.cost
      if player.tokens > cost
        puts "#{player.color} takes the shortcut!"
        player.square = player.square.next.first
        player.square.process_player player
      else
        # long way
        puts "#{player.color} could not afford the shortcut!"
        player.square = player.square.next.last
      end
    end
  end

end

class Die
  def self.roll
    rand 1..6
  end
end


# questions to answer
# does everyone always have enough tokens to win?
# does losing a turn automatically mean a loss
# does the shortcut cause you to automatically win/lose?
# does one "steal" win the game?


if $0 == __FILE__
  num_players = 2
  players = [
    Turtle.new(color: :red),
    Turtle.new(color: :green),
    Turtle.new(color: :blue),
    Turtle.new(color: :purple),
  ]
  b = Board.generate(
    size: 40,
    players: players,
    token_perc: 20, token_range: [1,1,1,1,1,1,1,2,2,3],
    lose_a_turn_perc: 7 ,
    shortcut_info: [20..30, 5],
    shortcut_cost: 5
  )

  g = Game.new(board: b, players: players)
  result = g.play_to_completion!
  b.print
  puts players.map{|pl| "#{pl.color} (#{pl.tokens})"}.join(" ")
end
