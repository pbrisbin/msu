module MSU
  class LineParser
    class ParseFail < StandardError
      def initialize(line, pattern)
        super("Parse fail: expected `#{line}' to match `#{pattern.inspect}'")
      end
    end

    def initialize(pattern)
      @pattern = pattern
    end

    def can_parse?(line)
      @pattern =~ line
    end

    def parse(line)
      match = @pattern.match(line) or parse_fail(line)

      yield(match)
    end

    private

    def parse_fail(line)
      raise ParseFail.new(line, @pattern)
    end
  end
end
