#!/usr/bin/ruby -w

#The Hello Class
class Hello
  # Define constructor for the class
  def initialize( name )
         @name = name.capitalize
  end

  # Define a ruby method
  def salute
         puts "Hello #{@name}!"
  end
end

# Create a new object for Hello class
obj = Hello.new("Ruby")

# Call ruby method
obj.salute
