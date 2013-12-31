module MSU
  class Command
    def initialize(displays)
      @primary, *@connected = displays.connected
      @disconnected = displays.disconnected
      @xrandr_arguments = ['xrandr']
    end

    def extend_right
      connected.reduce(primary) do |last, display|
        off(display).on(display).right_of(last)

        display
      end

      self
    end

    def all_off
      disconnected.each { |display| off(display) }

      self
    end

    def off(display)
      add "--output #{display.name} --off"
    end

    def on(display)
      add "--output #{display.name}"
      add "--mode #{display.modes.first}"
    end

    def right_of(display)
      add "--right-of #{display.name}"
    end

    def to_s
      xrandr_arguments.join(' ')
    end

    private

    attr_reader :primary, :connected, :disconnected, :xrandr_arguments

    def add(command)
      xrandr_arguments << command

      self
    end
  end
end
