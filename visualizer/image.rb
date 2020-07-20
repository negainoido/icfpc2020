require 'stringio'
class Image
	def initialize(state)
		@state = state
		@role2machines = Hash.new { |h, k| h[k] = [] }
		@state["ship_and_commands"].flatten.each do |m|
			@role2machines[m["role"]] << m if m.class == Hash && m["role"] # ignore "(nil) (niL)"
		end
	end

	def to_s()
		io = StringIO.new

		io.puts 'set object 1 rect from -16,-16 to 16,16 fc rgb "white" behind'
		io.puts "set xrange [-130:130]"
		io.puts "set yrange [-130:130]"
		io.puts "set size ratio -1"

		@role2machines.each do |role, machines|
			io.puts "$#{role} << EOD"
			p machines
			machines.each do |m|
				pos = m["position"]
				io.puts "#{pos[0]} #{pos[1]}"
			end
			io.puts "EOD"
		end

		data = @role2machines.map {|role, machines| "$#{role}"}.join(",")
		io.puts "plot #{data}"

		io.rewind
		return io.read
	end
end
