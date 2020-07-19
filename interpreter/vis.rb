require 'json'
require 'pp'
require 'fileutils'
require_relative 'json_to_ppm'
require 'stringio'
require 'open3'


class HistoryPrev < StandardError
end
class HistoryNext < StandardError
end

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
	if $options[:local]
		$stderr.puts "### running autotaker in local ###"
		galaxy = File.open("galaxy.txt").read()
		galaxy.gsub!(/^(:2000 = )(.*)$/, "\\1#{point}")
		galaxy.gsub!(/^(:2001 = )(.*)$/, "\\1#{data}")
		submit_data = galaxy
		cmd = "cabal new-exec interpreter"
	else
		$stderr.puts "### running autotaker in remote ###"
		submit_data = JSON.generate({ "galaxyState" => data, "galaxyArg" => point})
		cmd = "curl https://interpreter-w4qijdmu3q-an.a.run.app -H 'Content-Type: application/json' -d @-"
	end
	json_text = Open3.popen3(cmd) do |stdin, stdout, stderr, wait_thr|
		stdin.puts submit_data
		stdin.close
		#File.open("laststderr.txt", "w") {|f| f.write stderr.read}
		stdout.read()
	end
	return json_text
end

def plot_string_from(images, options = {})
	io = StringIO.new

	io.puts "unset object 1"
	if options[:click]
		pt = options[:click]
		io.puts "set object 1 circle at first #{pt[0]},#{pt[1]} radius char 0.7 front fillstyle pattern 1 border lc rgb '#ff0000' lw 2"
	end

	io.puts "set yrange [:] reverse"

	images.length.times do |i|
		io.puts "$image#{i} << EOD"
		images[i].each do |x, y|
			io.puts "#{x} #{y}"
		end
		io.puts "EOD"
	end

	data = images.length.times.map {|i| "$image#{i} pt 5 ps 0.7"}.join(",")
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
	images = images.select{|image| !image.empty?}

	@plot.puts plot_string_from(images)

	@plot.puts "set mouse verbose"

	ignore_inputs(@plot)
	# ignore_inputs($stdin)

	did = 0
	while true
		rs = IO.select([@plot, $stdin], [], [], did)
		did = nil
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
				$stderr.puts "Command: fix X Y"
				$stderr.puts "Command: stop"
				$stderr.puts "Command: back (or b)"
				$stderr.puts "Command: next (or n)"
			when /put `\s*(-?[-0-9\.]*),\s*(-?[0-9\.]*)' to clipboard\./i
				x = $1.to_f.round
				y = $2.to_f.round
				$stderr.puts "Clicked: #{x} #{y}"
				@plot.puts plot_string_from(images, {:click => [x, y]})
				return [x, y]
			when /stop/i
				@point_choicer = nil
			when /random walk/i
				@point_choicer = lambda {|images|
					random_point = images.select{|x| !x.empty?}.sample.sample
					random_point
				}
			when /fix\s*(.*)\s*(.*)/i
				x = $1.to_i
				y = $2.to_i
				@point_choicer = lambda {|images|
					[x, y]
				}
			when /\Ab\Z|back/i
				raise HistoryPrev
			when /\An\Z|next/i
				raise HistoryNext
			end
		end if rs

		break if @point_choicer
	end

	point = @point_choicer.call(images)
	$stderr.puts "point_choicer: choose #{point}"
	@plot.puts plot_string_from(images, {:click => point})
	return point
end

def save_images_as_png(images, as)
	IO.popen("gnuplot", "r+", :err => [:child, :out]) do |plot|
		plot.puts "set terminal png"
		plot.puts "set output '#{as}'"
		plot.puts plot_string_from(images)
	end
end

# pid + now
def filename_of_now()
	"#{Process.pid}_" + ("%10.9f" % Time.now.to_f).gsub(/\./, "_")
end

# return filename
def save_data(json, fileid)
	json["logVersion"] = 1.0
	filename = "./log/#{fileid}.json"
	FileUtils.mkdir_p('./log/')
	File.open(filename, "w") do |f|
		JSON.dump(json, f)
	end

	images = json["imageList"]
	if images 
		save_images_as_png(images, "./log/#{fileid}.png")

		File.open("./log/#{fileid}.ppm", "w") do |f|
			f.write ppm_from_images(images)
		end
	end

	$stderr.puts "Log is written as #{filename}"
end

def load_data(file)
	json = JSON.load(File.open(file).read)
	json
end

def load_data(file)
	json = JSON.load(File.open(file).read)
	json
end

def operate(next_point, data, last_filename = nil)
	result = res["returnValue"]
	data = res["stateData"]

	if result == 0
		# show images
		next_point = plot_and_interact(res["imageList"]).reverse
		next_point = point_to_lambda(next_point[0], next_point[1])
	else
		# interact with galaxy
		$stderr.puts "Interacting with Galaxy..."
		send_data = res["imageListAsData"] # "ap ap cons 0 nil"
		$stderr.puts "send_data: #{send_data}"
		send_data = modulate(send_data)
		$stderr.puts "modulated: #{send_data}"
		res = `curl -X POST "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=9ffa61129e0c45378b01b0817117622c" -H "accept: */*" -H "Content-Type: text/plain" -d "#{send_data}"`
		$stderr.puts "Response From Galaxy: #{res}"
		next_point = demodulate(res)
		$stderr.puts "Next Point: #{next_point}"
	end

	if previous_fileid
		json["previousFileID"] = previous_fileid
	end

	return next_filename
end


#next_point = point_to_lambda(0, 0)
#data = "nil"
#
#next_point = point_to_lambda(1, 4)
#data = "ap ap cons 2 ap ap cons ap ap cons 1 ap ap cons -1 nil ap ap cons 0 ap ap cons nil nil"
#
#point = point_to_lambda(-3, 1)
#data = "ap ap cons 2 ap ap cons ap ap cons 1 ap ap cons -1 nil ap ap cons 0 ap ap cons nil nil"
#
#
#
#next_point = point_to_lambda(-3, 1)
#data = "ap ap cons 2 ap ap cons ap ap cons 1 ap ap cons -1 nil ap ap cons 0 ap ap cons nil nil"

state = {
	"point" => point_to_lambda(0, 0),
	"data" => "nil"
}
state = {
	"point" => point_to_lambda(-3, 1),
	"data" => "ap ap cons 2 ap ap cons ap ap cons 1 ap ap cons -1 nil ap ap cons 0 ap ap cons nil nil"
}

require 'optparse'
opt = OptionParser.new

$options = {:local => false}
opt.on('-l', '--local', 'use api') {|v| $options[:local] = true}
opt.on('-f', '--file FILENAME', 'load from FILENAME') {|v| $options[:file] = v}

opt.parse!(ARGV)

if $options[:file]
	file = $options[:file]
	if !File.exists?(file)
		$stderr.puts "No such a file: #{file}"
		exit 1
	end
	$stderr.puts "Loading file: #{file}"
	state = load_data(file)
end



@plot = IO.popen("gnuplot", "r+", :err => [:child, :out])
@point_choicer = nil

last_filename = nil

history = []
future = []

while true
	json_text = exec_autotaker(state["point"], state["data"])
	# $stderr.puts json_text
	# $stderr.puts "done"

	begin
		res = JSON.parse(json_text)
	rescue JSON::ParserError => e
		$stderr.puts "Parse Error: #{e}"
		exit 1
	end

	res["point"] = state["point"]
	res["data"] = state["data"]
	if last_filename
		res["previousFileID"] = last_filename
	end

	next_filename = filename_of_now()
	res["FileID"] = next_filename
	#save_data(next_point, data, res, next_filename, last_filename)
	save_data(res, next_filename)
	last_filename = next_filename

	result = res["returnValue"]
	data = res["stateData"]
	res["data"] = data

	if result == 0
		# show images
		while true
			begin
				next_point = plot_and_interact(res["imageList"])
				next_point = point_to_lambda(next_point[0], next_point[1])
				break
			rescue HistoryPrev
				if history.empty?
					$stderr.puts "there is no previous history"
					next
				end
				next_state = history.pop
				future.push res.clone
				res = next_state
				$stderr.puts "CurrentID: #{res["FileID"]}"
				next
			rescue HistoryNext
				if future.empty?
					$stderr.puts "there is no next history"
					next
				end
				next_state = future.pop
				history.push res.clone
				res = next_state
				$stderr.puts "CurrentID: #{res["FileID"]}"
				next
			end
		end

		history << res.clone
		future = []
	else
		# interact with galaxy
		$stderr.puts "Interacting with Galaxy..."
		send_data = res["imageListAsData"] # "ap ap cons 0 nil"
		# $stderr.puts "send_data: #{send_data}"
		send_data = modulate(send_data)
		# $stderr.puts "modulated: #{send_data}"
		response = `curl -X POST "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=9ffa61129e0c45378b01b0817117622c" -H "accept: */*" -H "Content-Type: text/plain" -d "#{send_data}"`
		# $stderr.puts "Response From Galaxy: #{response}"
		next_point = demodulate(response)
		# $stderr.puts "Next Point: #{next_point}"
	end
	res["point"] = next_point

	state = res
end


