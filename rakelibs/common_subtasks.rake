# This way of requiring avoids double loading
require File.expand_path('./rakehelpers.rb', File.dirname(__FILE__))
require 'rake'
require 'rake/clean'


# ==========================
# directory tasks
# ==========================

# making tasks not recompile themselves everytime
class Rake::Task
  def timestamp
    prerequisite_tasks.collect { |pre| pre.timestamp }.max || Time.now
  end
end


CLEAN.include("#{BUILD_DIR}**")
CLOBBER.include("#{ARM_BUILD_DIR}**", "#{TEST_BUILD_DIR}**", "#{SOURCEGEN_DIR}**")
