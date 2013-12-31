require 'msu/command'
require 'msu/displays'
require 'msu/hook'

module MSU
  class Client
    attr_reader :command

    def initialize(xrandr_output)
      @command = Command.new(Displays.parse(xrandr_output))
    end

    def run(argv)
      command.all_off.extend_right

      puts "#{command}"
      system "#{command}"

      Hook.new('after-setup').run
    end
  end
end
