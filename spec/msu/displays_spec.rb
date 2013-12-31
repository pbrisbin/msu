require 'spec_helper'

describe MSU::Displays do
  context "#disconnected" do
    it "returns all disconnected displays" do
      disconnected = displays.disconnected

      expect(disconnected.map(&:name)).to eq(
        %w(VGA1 HDMI1 DP1 HDMI2 HDMI3 DP2 DP3)
      )
    end
  end

  context "#connected" do
    it "returns connected displays with modes" do
      connected = displays.connected

      expect(connected.map(&:name)).to eq ['LVDS1']
      expect(connected.first).to have(4).modes
    end
  end

  private

  def displays
    described_class.parse <<EOM
Screen 0: minimum 320 x 200, current 1600 x 900, maximum 32767 x 32767
LVDS1 connected 1600x900+0+0 (normal left inverted right x axis y axis) 309mm x 174mm
   1600x900       60.0*+   40.0  
   1024x768       60.0  
   800x600        60.3     56.2  
   640x480        59.9  
VGA1 disconnected (normal left inverted right x axis y axis)
HDMI1 disconnected (normal left inverted right x axis y axis)
DP1 disconnected (normal left inverted right x axis y axis)
HDMI2 disconnected (normal left inverted right x axis y axis)
HDMI3 disconnected (normal left inverted right x axis y axis)
DP2 disconnected (normal left inverted right x axis y axis)
DP3 disconnected (normal left inverted right x axis y axis)
EOM
  end
end
