# Various modules to desire

# Weakly require
module Kernel
  def desire(library)
    begin
      require library
    rescue Exception
      nil
    end
  end
end

desire 'pp'
desire 'irb/completion'
desire 'enumerator'
desire 'active_support'
desire 'facet/symbol/to_proc'
