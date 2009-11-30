#\ -w -p 8765
use Rack::Reloader, 0
use Rack::ContentLength

require 'pp'
require 'hubris'
class Fibonacci 
  hubris :source => 'Fibonacci.hs'
end


def arg_from env
 #env['REQUEST_URI'] ? env['REQUEST_URI'].to_s.sub(/^\//, '').to_i : 0
 env['PATH_INFO'] ? env['PATH_INFO'].to_s.sub(/^\//, '').to_i : 0
end

app = proc do |env|
  value = Fibonacci.new.fibonacci( arg_from env )
  [ 200, {'Content-Type' => 'text/plain'}, "The fib number is #{value }" ]
  # [ 200, {'Content-Type' => 'text/plain'}, "The fib number is #{arg_from env}" ]
end

run app
