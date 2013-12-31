require 'spec_helper'

describe MSU::Display do
  context '.parse' do
    it "parses a disconnected display" do
      display = described_class.parse('LVDS1 disconnected (...)')

      expect(display.name).to eq 'LVDS1'
      expect(display.position).to be_nil
      expect(display.modes).to be_empty
      expect(display).not_to be_connected
    end

    it "parses a connected display" do
      display = described_class.parse('LVDS1 connected 1600x900+10+20 ...')

      expect(display.name).to eq 'LVDS1'
      expect(display.position).to eq MSU::Pos.new(10,20)
      expect(display.modes).to be_empty
      expect(display).to be_connected
    end

    it "consumes the display's modes up to a non-mode line" do
      mode_lines = [
        '   1600x900       60.0*+   40.0  ',
        '   1024x768       60.0  ',
        'VGA1 disconnected (...)'
      ]
      display = described_class.parse(
        'LVDS1 connected 1600x900+0+0 ...', mode_lines
      )

      expect(display.modes).to eq [
        MSU::Mode.new(1600, 900, true, true),
        MSU::Mode.new(1024, 768, false, false)
      ]
      expect(mode_lines).to eq ['VGA1 disconnected (...)']
    end

    it "raises on invalid input" do
      expect { described_class.parse_disconnected('...') }.to raise_error
    end
  end
end
