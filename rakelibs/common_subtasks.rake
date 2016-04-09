# This way of requiring avoids double loading
require File.expand_path('./rakehelpers.rb', File.dirname(__FILE__))
require 'rake'
require 'rake/clean'


# ==========================
# directory tasks
# ==========================

directory(BUILD_DIR) do |dir|
  mkdir_p dir.name
end

directory(ARM_BUILD_DIR) do |dir|
  mkdir_p dir.name
end

directory(TEST_BUILD_DIR) do |dir|
  mkdir_p dir.name
end

directory(SOURCEGEN_DIR) do |dir|
  mkdir_p dir.name
end

task :clean do |task|
  rm_rf ARM_BUILD_DIR
  rm_rf TEST_BUILD_DIR
end

# making tasks not recompile themselves everytime
class Rake::Task
  def timestamp
    prerequisite_tasks.collect { |pre| pre.timestamp }.max || Time.now
  end
end
