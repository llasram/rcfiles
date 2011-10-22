#
# MethodFinder
#

unless defined? MethodFinder

  class Object
    alias :clone_MethodFinder__ :clone

    def clone
      begin
        self.clone_MethodFinder__
      rescue TypeError
        self
      end
    end

    def what?(*a)
      MethodFinder.new(self, *a)
    end
    alias :which? :what?
  end

  class MethodFinder
    @@black_list = %w{
      fork daemonize which? what?
    }.collect! { |s| s.strip }

    def initialize(obj, *args)
      @obj = obj
      @args = args
    end
    
    def ==(val)
      MethodFinder.show(@obj, val, *@args)
    end

    def self.quietly
      verbose = $VERBOSE
      $VERBOSE = nil

      begin
        result = yield
      ensure
        $VERBOSE = verbose
      end

      return result
    end

    # Find all methods on [anObject] which, when called with [args] return
    # [expectedResult]
    def self.find(anObject, expectedResult, *args)
      quietly do
        anObject.methods.select do |name|
          arity = anObject.method(name).arity
          arity == -1 || arity == args.size
        end.select do |name|
          not @@black_list.include? name
        end.select do |name|
          begin
            anObject.clone.method(name).call(*args) == expectedResult
          rescue
          end
        end
      end
    end

    # Pretty-prints the results of the previous method
    def self.show(anObject, expectedResult, *args)
      find(anObject, expectedResult, *args).each do |name|
        print "#{anObject.inspect}.#{name}"
        unless args.empty?
          print "(" + args.map { |o| o.inspect }.join(", ") + ")"
        end
        puts " == #{expectedResult.inspect}"
      end
    end
  end

end
