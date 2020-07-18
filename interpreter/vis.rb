# modulate("nil") == "00"
# modulate("ap ap cons nil nil") == "110000"
# modulate("ap ap cons 0 nil") == "1101000"
# modulate("ap ap cons 1 2") == "110110000101100010"
# modulate("-1") == "10100001"
def modulate(s)
	def f(s)
		a = s.shift
		case a
		when "cons"
			left = f(s)
			right = f(s)
			return "11" + left + right
		when "nil"
			return "00"
		when "0"
			return "010"
		when /-?\d+/
			a = a.to_i
			sign = a >= 0 ? "01" : "10"
			a = a.abs
			bin = a.to_s(2)
			bin = bin.rjust((bin.length + 3) / 4 * 4, '0')
			return sign + "1" * (bin.length / 4) + "0" + bin
		else
			raise "Modulate match failed! #{s}"
		end
	end
	s = s.split(" ").select {|x| x != "ap"}
	f(s)
end

def demodulate(s)
	def f(s)
		prefix = s.shift + s.shift

		res = []
		case prefix
		when "00"
			res << "nil"
		when "11"
			res << "ap ap cons "
			res += f(s)
			res << " "
			res += f(s)
		else
			if prefix == "01"
				sign = 1
			else
				sign = -1
			end
			len = 0
			while s[0] != "0"
				len += 1
				s.shift
			end
			s.shift
			len *= 4
			num =  s[0..len-1].join("").to_i(2)
			res << sign * num
			len.times { s.shift }
		end
		res
	end
	s = s.split("")
	f(s).join().chomp(" ").chomp(" ")
end

def pt(x, y)
	[x, y]
end
def read_image_from_string(lines)
	lines.split("\n").each do |line|
		if line =~/\AImageList: (.*)/
			res = $1
			return eval(res.gsub(/\(/, 'pt('))
		end
	end
end

def write_out(image, rgb, opaq)
	image.each do |x, y|
		@pixels[ [x, y] ] = rgb.map {|c| (c * opaq).to_i}
	end
end

def exec_autotaker(point, data = "nil")
	galaxy = File.open("galaxy.txt").read()
	galaxy.gsub!(/^(:2000 = ap ap cons) (.*)$/, "\\1 #{point}")
	galaxy.gsub!(/^(:2001 = )(.*)$/, "\\1#{data}")
	puts "point: #{point}"
	puts "data: #{data}"

	lines = IO.popen(["cabal", "new-exec", "interpreter"], "r+") do |autotaker|
		autotaker.puts galaxy
		autotaker.close_write

		autotaker.read()
	end
end

def plot_and_interact(lines)
	images = read_image_from_string(lines)

	images.length.times do |i|
		@plot.puts "$image#{i} << EOD"
		images[i].each do |x, y|
			@plot.puts "#{x} #{y}"
		end
		@plot.puts "EOD"
	end

	data = images.length.times.map {|i| "$image#{i}"}.join(",")
	@plot.puts "plot #{data}"


	@plot.puts "set mouse verbose"

	while l = @plot.gets
		puts "gnuplot: #{l}"
		if l =~ /put `\s*(-?[-0-9\.]*),\s*(-?[0-9\.]*)' to clipboard\./
			x = $1.to_f.round
			y = $2.to_f.round
			puts "Clicked: #{x} #{y}"
			return [x, y]
		end
	end
end

next_point = "0 0"
data = "nil"

next_point = "1 4"
data = "ap ap cons 2 ap ap cons ap ap cons 1 ap ap cons -1 nil ap ap cons 0 ap ap cons nil nil"


#next_point = "-3 1"
#data = "ap ap cons 2 ap ap cons ap ap cons 1 ap ap cons -1 nil ap ap cons 0 ap ap cons nil nil"

@plot = IO.popen("gnuplot", "r+", :err => [:child, :out])

while true
	lines = exec_autotaker(next_point, data)
	puts "### autotaker"
	puts lines

	raise "Could not find Result from autotaker!" if lines !~ /Result: (.*)/
	result = $1.to_i
	raise "We could not find the next data from autotaker!" if lines !~ /DataAsCode: (.*)/
	data = $1

	if result == 0
		# show images
		next_point = plot_and_interact(lines)
		next_point = "#{next_point[0]} #{next_point[1]}"
	else
		# interact with galaxy
		puts "Interacting with Galaxy..."
		raise "Could not find ImageListAsCode!" if lines !~/\AImageListAsCode: (.*)/
		send_data = $1 # "ap ap cons 0 nil"
		send_data = modulate(send_data)
		res = `curl -X POST "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=9ffa61129e0c45378b01b0817117622c" -H "accept: */*" -H "Content-Type: text/plain" -d "#{send_data}"`
		$stderr.puts "Response From Galaxy: #{res}"
		res = demodulate(res)
		res.split(" ").select{|x| x =~ /\d/}.map {|x| x.to_i}
		$stderr.puts "Next Point: #{res}"
		next_point = "#{res[0]} ap ap cons #{res[1]} nil"
	end
end


exit


xmin = images.flatten(1).map {|x, y| x}.min
xmax = images.flatten(1).map {|x, y| x}.max
ymin = images.flatten(1).map {|x, y| y}.min
ymax = images.flatten(1).map {|x, y| y}.max

@pixels = Hash.new([0, 0, 0])

images.each do |image|
	write_out(image, [255, 255, 255], 1.0 / images.length)
end

width = xmax - xmin + 1
height = ymax - ymin + 1
puts "P3"
puts "#{width} #{height}"
puts "255"
for y in ymin..ymax
	for x in xmin..xmax
		c = @pixels[[x,y]]
		puts c.join(" ")
	end
end

