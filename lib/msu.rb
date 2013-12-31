require 'msu/command'
require 'msu/display'
require 'msu/displays'
require 'msu/line_parser'

module MSU
  class Client
    attr_reader :command

    def initialize(xrandr_output)
      @command = Command.new(Displays.parse(xrandr_output))
    end

    def run(argv)
      # TODO: support non-default actions
      command.all_off
      command.extend_right

      puts "#{command}"
      system "#{command}"
    end
  end
end
