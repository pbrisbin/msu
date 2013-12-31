module MSU
  Displays = Struct.new(:connected, :disconnected) do
    def self.parse(xrandr_output)
      xrandr_lines = xrandr_output.split("\n")
      xrandr_lines.shift # discard Screen

      displays = []

      while line = xrandr_lines.shift do
        displays << Display.parse(line, xrandr_lines)
      end

      new(*displays.partition(&:connected?))
    end
  end
end
