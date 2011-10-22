class Object
  def interesting_methods
    base = [ Class, Module ].include?(self.class) ? self.class : Object.new
    self.methods.sort - base.methods
  end
end
