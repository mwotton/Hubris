require 'dl/import'

module HaskyPants
  if /^1\.8/ =~ RUBY_VERSION
    extend DL::Importable
  else
    extend DL::Importer
  end

  dlload "./libdynhs.so"
  ["int fibonacci_hs(int)"].each do |f|
    extern f
  end
end

puts HaskyPants.fibonacci_hs(12)
