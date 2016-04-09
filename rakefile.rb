require 'rake'
require 'rake/clean'
import './rakelibs/test_tasks.rake'
import './rakelibs/arm_tasks.rake'
require File.expand_path('./rakelibs/rakehelpers.rb', File.dirname(__FILE__))
require File.expand_path('./rakelibs/test_target_helpers.rb', File.dirname(__FILE__))

#Rake.application.options.trace_rules = true

task :arm => elf_hex_bin_files(:arm)

task :default => :test

task :test => get_tests


