# Continuous test runner, Craig Riecke, CoSciN Programmer/Analyst, Oct. 2015
# Requires watchr and colorize (install via "sudo gem install watchr colorize") 
#
# Run this script in a terminal window with:
#      cd lib_test; watchr continuous_test_runner.watchr
#
# Then change Frenetic lib/* or lib_test/* files and "make test" will be run each time.  
# Saves lotsa keystrokes when you're hacking away at writing unit tests.  Note: this uses
# polling, so can be tough on the CPU.  Inotify is more intelligent, but doesn't work
# with VirtualBox shared file systems, which don't send inotify event.  Bummer.  
#

require 'colorize'

watch( '(.*)\.ml' ) do 
  puts "[#{Time.now}] Running tests".yellow; 
  out = `cd .. ; make test 2>&1` 
  puts out
  puts ($?.to_i != 0) ? "[#{Time.now}] TESTS FAILED!".red : "[#{Time.now}] Tests passed.".green
end