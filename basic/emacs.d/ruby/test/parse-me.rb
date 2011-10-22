module Outer
  class << self
    def example(with, args)
      if arg
        begin
          dance
        rescue
        end until over
        puts $:
        $# = $/
      end
    end

    def more_and_mare
      begin
        laugh
      rescue Exception
        cry
      end
    end

    def temple_of_doom(one, two, &block_my_shoe)
      begin
        scream
      ensure
        rescued! :by => IndianJones
      end
    end
  end

  class Inner
    def exemplar
      case silly
      when true; dance
      when false
        run_away
      else
        crush_enemies
      end
    end
  end

  def doit(*args)
    args.each do |arg|
      puts <<-end_here_doc
        My favorite arg is '#{arg}'.
      end_here_doc
    end
  end

  def delimitate
    general = %q{ matching }
    corporal = %r| weapons |
    sergeant = %q^ Well, this is odd ^
    major = %w(seriously annoying)
  end

  def quotate
    puts [ ?', ?", ?` ]
  end

  def finalize song, dance, war
    "This is the end"
  end
end
