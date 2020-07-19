require 'json'
require 'pp'
require 'fileutils'

def point_to_lambda(x, y)
	"ap ap cons #{x} #{y}"
end
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
      if len == 0
        res << 0
      else
        len *= 4
        num =  s[0..len-1].join("").to_i(2)
        res << sign * num
        len.times { s.shift }
      end
		end
		res
	end
	s = s.split("")
	f(s).join().chomp(" ").chomp(" ")
end

=begin
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
=end

def exec_autotaker(point, data = "nil")
	galaxy = File.open("galaxy.txt").read()
	galaxy.gsub!(/^(:2000 = )(.*)$/, "\\1#{point}")
	galaxy.gsub!(/^(:2001 = )(.*)$/, "\\1#{data}")
	puts "point: #{point}"
	puts "data: #{data}"

	lines = IO.popen(["cabal", "new-exec", "interpreter"], "r+") do |autotaker|
		autotaker.puts galaxy
		autotaker.close_write

		autotaker.read()
	end
end

def plot_string_from(images)
	io = StringIO.new
	images.length.times do |i|
		io.puts "$image#{i} << EOD"
		images[i].each do |x, y|
			io.puts "#{x} #{y}"
		end
		io.puts "EOD"
	end

	data = images.length.times.map {|i| "$image#{i}"}.join(",")
	io.puts "plot #{data}"

	io.rewind
	io.read
end

def ignore_inputs(io)
	begin
		while true
			io.read_nonblock(100)
		end
	rescue IO::EAGAINWaitReadable
	end
end

def plot_and_interact(images)
	@plot.puts plot_string_from(images)


	@plot.puts "set mouse verbose"

	ignore_inputs(@plot)
	ignore_inputs($stdin)

	while true
		break if @random_walk

		rs = IO.select([@plot, $stdin])
		rs[0].each do |io|
			l = io.gets

			if io == @plot
				$stderr.puts "gnuplot: #{l}"
			else
				$stderr.puts "stdin: #{l}"
			end
			case l
			when /help/i
				$stderr.puts "help (Command list)"
				$stderr.puts "Command: put `X, Y' to clipboard\."
				$stderr.puts "Command: random walk"
			when /put `\s*(-?[-0-9\.]*),\s*(-?[0-9\.]*)' to clipboard\./i
				x = $1.to_f.round
				y = $2.to_f.round
				$stderr.puts "Clicked: #{x} #{y}"
				return [x, y]
			when /random walk/i
				@random_walk = true
			end
		end
	end

	if @random_walk
		random_point = images.sample.sample
		$stderr.puts "random walk: choose #{random_point}"
		return random_point
	end
end

def save_images_as_png(images, as)
	IO.popen("gnuplot", "r+", :err => [:child, :out]) do |plot|
		plot.puts "set terminal png"
		plot.puts "set output '#{as}'"
		plot.puts plot_string_from(images)
	end
end

def save_data(point, data, json)
	json = json.clone
	json["point"] = point
	json["data"] = data
	fileprefix = ("%10.9f" % Time.now.to_f).gsub(/\./, "_")


	FileUtils.mkdir_p('./log/')
	File.open("./log/#{fileprefix}.json", "w") do |f|
		JSON.dump(json, f)
	end

	images = json["imageList"]
	save_images_as_png(images, "./log/#{fileprefix}.png") if images
end

def load_data(file)
	json = JSON.load(File.open(file).read)
	json
end

#next_point = point_to_lambda(0, 0)
#data = "nil"
#
#next_point = point_to_lambda(1, 4)
#data = "ap ap cons 2 ap ap cons ap ap cons 1 ap ap cons -1 nil ap ap cons 0 ap ap cons nil nil"

next_point = point_to_lambda(-3, 1)

data = "ap ap cons 2 ap ap cons ap ap cons 1 ap ap cons -1 nil ap ap cons 0 ap ap cons nil nil"
#
#
#next_point = point_to_lambda(-3, 1)
#data = "ap ap cons 2 ap ap cons ap ap cons 1 ap ap cons -1 nil ap ap cons 0 ap ap cons nil nil"


if ARGV.length > 0
	file = ARGV[0]
	if !File.exists?(file)
		$stderr.puts "No such a file: #{file}"
		exit 1
	end
	$stderr.puts "Loading file: #{file}"
	json = load_data(file)
	next_point = json["point"]
	data = json["data"]
end


@plot = IO.popen("gnuplot", "r+", :err => [:child, :out])
@random_walk = false

while true
	lines = exec_autotaker(next_point, data)
	$stderr.puts "### autotaker ###"
	$stderr.puts lines
	$stderr.puts "#################"

	json = File.open("result.json").read()
	res = JSON.parse(json)

	save_data(next_point, data, res)

	result = res["returnValue"]
	data = res["stateData"]



	if result == 0
		# show images
		next_point = plot_and_interact(res["imageList"])
		next_point = point_to_lambda(next_point[0], next_point[1])
	else
		# interact with galaxy
		puts "Interacting with Galaxy..."
		send_data = res["imageListAsData"] # "ap ap cons 0 nil"
		$stderr.puts "send_data: #{send_data}"
		send_data = modulate(send_data)
		$stderr.puts "modulated: #{send_data}"
		res = `curl -X POST "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=9ffa61129e0c45378b01b0817117622c" -H "accept: */*" -H "Content-Type: text/plain" -d "#{send_data}"`
		$stderr.puts "Response From Galaxy: #{res}"
		next_point = demodulate(res)
		$stderr.puts "Next Point: #{next_point}"
	end
end


exit


=begin
def write_out(image, rgb, opaq)
	image.each do |x, y|
		@pixels[ [x, y] ] = rgb.map {|c| (c * opaq).to_i}
	end
end


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
=end
