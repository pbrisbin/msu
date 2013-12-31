require 'spec_helper'

describe MSU::Command do
  context "#all_off" do
    it "disables all disconnected inputs" do
      displays = MSU::Displays.new
      displays.disconnected = [
        MSU::Display.new("VGA1"),
        MSU::Display.new("HDMI1"),
        MSU::Display.new("HDMI2")
      ]

      xrandr = described_class.new(displays).all_off.to_s

      expect(xrandr).to include('--output VGA1 --off')
      expect(xrandr).to include('--output HDMI1 --off')
      expect(xrandr).to include('--output HDMI2 --off')
    end
  end

  context '#extend_right' do
    it "enables non-primary displays at first mode, each right of the last" do
      displays = MSU::Displays.new
      displays.connected = [
        display('LVDS1', mode(1600, 900),  mode(1000, 800)),
        display('VGA1',  mode(1920, 1600), mode(1000, 800)),
        display('HDMI1', mode(2800, 1400), mode(1000, 800))
      ]

      xrandr = described_class.new(displays).extend_right.to_s

      expect(xrandr).to include('--output VGA1 --off')
      expect(xrandr).to include('--output VGA1 --mode 1920x1600 --right-of LVDS1')
      expect(xrandr).to include('--output HDMI1 --off')
      expect(xrandr).to include('--output HDMI1 --mode 2800x1400 --right-of VGA1')
    end

    private

    def display(name, *modes)
      display = MSU::Display.new(name, MSU::Pos.new(0, 0))
      display.stub(:modes).and_return(modes)

      display
    end

    def mode(width, height)
      MSU::Mode.new(width, height, false, false)
    end
  end
end
