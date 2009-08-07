require 'dl/import' 

module HaskyPants
  extend DL::Importable
  dlload "./libdynhs.so"
  extern "int fibonacci_hs(int)"
end

puts HaskyPants.fibonacci_hs(12)
