require 'optparse'

class TestRunnerGenerator

  def initialize(test_file, preamble, postamble)
    @test_file = test_file
    @tests = Hash.new
    _process_test_file
    @preamble = preamble
    @postamble = postamble
  end

  def result
    result = []
    result << [@preamble.dup]
    @tests.each do |group, functions|
      result << _build_group_header(group)

      functions.each do |function|
        result << _build_test_case_runner(group, function)
      end

      result << _build_group_footer(group)
    end
    result << _build_runner_functions(@tests.keys)
    result.concat [@postamble.dup]

    return result.join("\n")
  end

  def _process_test_file
    @test_file.each_line.each do |line|
      test_declaration = _extract_test_declaration(line)
      if test_declaration != nil
        group, function = test_declaration.captures
        _store_test_declaration(group, function)
      end
    end
  end

  def _build_group_header(group)
    return "TEST_GROUP_RUNNER(#{group}){"
  end

  def _build_group_footer(group)
    return "}"
  end

  def _build_test_case_runner(group,function)
    return "RUN_TEST_CASE(#{group}, #{function});"
  end

  def _build_runner_functions(groups)
    result = []
    result << "static void run_all_tests(void){"

    groups.each do |group|
      result << _build_runner_function(group)
    end

    result << "}"
    return result.join("\n").to_s
  end

  def _build_runner_function(group)
    return "RUN_TEST_GROUP(#{group});"
  end

  def _extract_test_declaration(source_line)
    source_line = source_line.encode("UTF-16be", :invalid=>:replace, :replace=>"?").encode('UTF-8')
    return source_line.match(/\s*TEST\s*\(\s*(?<group>\w+)\s*,\s*(?<function>\w+)\s*\).*/)
  end

  def _store_test_declaration(group, function)
    if @tests[group] == nil
      @tests[group] = []
    end

    @tests[group] << function
  end

end

class TestRunnerGeneratorCmd

  def initialize()
    @arguments = Hash.new
    @preamble
    @postamble
  end

  def run()
    _parse_command_line
    _prepare_template
    _generate_test_runner
  end

  def _parse_command_line
    OptionParser.new do |opts|
      opts.on('-o','--output OUTPUT',
      "Name of the generated OUTPUT File") do |out|
        @arguments[:output_path] = out
      end
      opts.on('-t','--template TEMPLATE',
      "TEMPLATE for the Runner") do |template|
        @arguments[:template] = template
      end
      opts.on('-r','--require TESTFILE',
      "File with the TESTFILE, the test runner should generate") do |test_file|
        @arguments[:test_file] = test_file
      end
    end.parse!
  end

  def set_test_file(test_file)
       @arguments[:test_file] = test_file
  end

  def set_output_path(out)
       @arguments[:output_path] = out
  end

  def set_template(template)
    @arguments[:template] = template
  end

  def _prepare_template
    template_file = File.open(@arguments[:template], 'r').read
    template = template_file.split('//CONTENT')

    if template.length < 2
      msg = "Template does not contain the '//CONTENT' placeholder"
      raise(msg)
    end

    if template.length > 2
      msg = "Template contains multiple '//CONTENT' placeholders"
      raise(msg)
    end

    @preamble = template[0]
    @postamble = template[1]
  end

  def _generate_test_runner
    test_file = open(@arguments[:test_file],'r').read
    output_file = open(@arguments[:output_path], 'w')

    generator = TestRunnerGenerator.new(test_file, @preamble, @postamble)

    output_file.write(generator.result)

  end
end

cmd = TestRunnerGeneratorCmd.new()
cmd.run
