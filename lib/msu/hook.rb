module MSU
  class Hook
    attr_reader :file

    def initialize(name)
      @file = File.join(xdg_config_home, 'msu', name)
    end

    def run
      system(file) if File.executable?(file)
    end

    private

    def xdg_config_home
      ENV.fetch('XDG_CONFIG_HOME') do
        File.join(ENV['HOME'], '.config')
      end
    end
  end
end
