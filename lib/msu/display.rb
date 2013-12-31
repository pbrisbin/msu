require 'msu/mode'
require 'msu/pos'

module MSU
  class Display
    attr_reader :name, :position, :modes

    def self.parse(xrandr_line, rest = [])
      parser = LineParser.new(/^(\S+) disconnected /)

      if parser.can_parse?(xrandr_line)
        return parser.parse(xrandr_line) { |match| new(match[1]) }
      end

      parser = LineParser.new(/^(\S+) connected \d+x\d+\+(\d+)\+(\d+)/)
      parser.parse(xrandr_line) do |match|
        new(match[1], Pos.new(match[2].to_i, match[3].to_i), rest)
      end
    end

    def initialize(name, position = nil, mode_lines = [])
      @name = name
      @position = position
      @modes = []

      parse_modes(mode_lines)
    end

    def connected?
      !position.nil?
    end

    def parse_modes(xrandr_lines)
      while line = xrandr_lines.shift
        mode = Mode.parse(line) or break
        modes << mode
      end

      xrandr_lines.unshift(line) if line
    end
  end
end
