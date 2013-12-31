# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'msu/version'

Gem::Specification.new do |spec|
  spec.name          = "msu"
  spec.version       = MSU::VERSION
  spec.authors       = ["Pat Brisbin"]
  spec.email         = ["pbrisbin@gmail.com"]
  spec.description   = %q{Monitor Setup Utility}
  spec.summary       = %q{A convenient and automated wrapper over xrandr}
  spec.homepage      = "https://github.com/pbrisbin/msu"
  spec.license       = "MIT"

  spec.files         = `git ls-files`.split($/)
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^spec/})
  spec.require_paths = ["lib"]

  spec.add_runtime_dependency 'bundler'
end
