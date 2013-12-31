module MSU
  Mode = Struct.new(:width, :height, :current, :preferred) do
    def self.parse(xrandr_line)
      current = xrandr_line.include?('*')
      preferred = xrandr_line.include?('+')
      parser = LineParser.new(/^\s+(\d+)x(\d+)/)
      parser.parse(xrandr_line) do |match|
        new(match[1].to_i, match[2].to_i, current, preferred)
      end
    rescue LineParser::ParseFail
    end

    def to_s
      "#{width}x#{height}"
    end
  end
end
