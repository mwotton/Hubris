#\ -w -p 8765
use Rack::Reloader, 0
use Rack::ContentLength

require 'pp'
require 'dl/import' 

module HaskyPants
  extend DL::Importable
  dlload "./libdynhs.so"
  extern "int fibonacci_hs(int)"
end


def arg_from env
 env['REQUEST_URI'] ? env['REQUEST_URI'].to_s.sub(/^\//, '').to_i : 0
end

app = proc do |env|
  value = HaskyPants.fibonacci_hs( arg_from env )
  [ 200, {'Content-Type' => 'text/plain'}, "The fib number is #{value }" ]
end

run app
