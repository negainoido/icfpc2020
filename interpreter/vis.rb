#!/usr/bin/env ruby
require 'json'
require 'pp'
require 'fileutils'
require_relative 'json_to_ppm'
require 'stringio'
require 'open3'

class HistoryPrev < StandardError; end
class HistoryNext < StandardError; end
class SaveAs < StandardError; end

def point_to_lambda(x, y)
	"ap ap cons #{x} #{y}"
end
def lambda_to_point(s)
	raise "failed to lambda to point" if s !~ /ap ap cons (-?\d+) (-?\d+)/
	[$1.to_i, $2.to_i]
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
	io.puts "set size ratio -1"

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
				case l
				when /\Aput `\s*(-?[-0-9\.]*),\s*(-?[0-9\.]*)' to clipboard\./i
					x = $1.to_f.round
					y = $2.to_f.round
					$stderr.puts "Clicked: #{x} #{y}"
					@plot.puts plot_string_from(images, {:click => [x, y]})
					return [x, y]
				end
			else
				$stderr.puts "stdin: #{l}"
				case l
				when /\Ahelp/i
					$stderr.puts "help (Command list)"
					$stderr.puts "Command: click X Y"
					$stderr.puts "Command: random walk"
					$stderr.puts "Command: fix X Y"
					$stderr.puts "Command: stop"
					$stderr.puts "Command: back (or b, prev, p)"
					$stderr.puts "Command: next (or n)"
					$stderr.puts "Command: save as FILENAME"
				when /\Aclick\s+(-?\d+)\s+(-?\d+)/
					x = $1.to_i
					y = $2.to_i
					$stderr.puts "Clicked: #{x} #{y}"
					@plot.puts plot_string_from(images, {:click => [x, y]})
					return [x, y]
				when /\Astop/i
					@point_choicer = nil
				when /\Arandom walk/i
					@point_choicer = lambda {|images|
						random_point = images.select{|x| !x.empty?}.sample.sample
						random_point
					}
				when /\Afix\s*(-?\d+)\s*(-?\d+)/i
					x = $1.to_i
					y = $2.to_i
					@point_choicer = lambda {|images|
						[x, y]
					}
				when /\Asave\s*as\s+(.*)/
					raise SaveAs, $1
				when /\Ab\Z|back|\Ap\Z|prev/i
					raise HistoryPrev
				when /\An\Z|next/i
					raise HistoryNext
				else
					$stderr.puts "Unknown command: #{l}"
				end
			end
		end if rs

		if @point_choicer
			point = @point_choicer.call(images)
			if point
				$stderr.puts "point_choicer: choose #{point}"
				@plot.puts plot_string_from(images, {:click => point})
				return point
			end

			@point_choicer = nil # end of point choicer
		end
	end
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
def save_data(json, filename = nil)
	json["logVersion"] = 2
	fileid = json["FileID"]
	filename = "./log/#{fileid}.json" if !filename
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
state = {
    "point" => "ap ap cons 1 ap ap cons 5700 nil",
    "data" => "ap ap cons 5 ap ap cons ap ap cons 1 ap ap cons 0 ap ap cons nil ap ap cons nil ap ap cons nil ap ap cons nil ap ap cons nil ap ap cons 0 nil ap ap cons 9 ap ap cons nil nil"
}


require 'optparse'
opt = OptionParser.new

$options = {:local => false}
opt.on('-l', '--local', 'use api') {|v| $options[:local] = true}
opt.on('-f', '--file FILENAME', 'load from FILENAME') {|v| $options[:file] = v}
opt.on('-c', '--clickhistory FILENAME', 'load click history (.json) and play') {|v| $options[:clickHistory] = v}

opt.parse!(ARGV)


@plot = IO.popen("gnuplot", "r+", :err => [:child, :out])
@point_choicer = nil



if $options[:file]
	file = $options[:file]
	if !File.exists?(file)
		$stderr.puts "No such a file: #{file}"
		exit 1
	end
	$stderr.puts "Loading file: #{file}"
	state = JSON.load(File.open(file).read)
end

clickHistory = []
if $options[:clickHistory]
	$stderr.puts "Warning: ignoring file option" if $options[:file]
	file = $options[:clickHistory]
	state = JSON.load(File.open(file).read)
	clickHistory = state["clickHistory"]
	@point_choicer = lambda {|images|
		if clickHistory.empty?
			nil
		else
			lambda_to_point(clickHistory.shift)
		end
	}
	state = state["initialState"]
end

state["clickHistory"] = []
state["initialState"] = state.clone

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
	res["clickHistory"] = state["clickHistory"]
	res["initialState"] = state["initialState"]
	if last_filename
		res["previousFileID"] = last_filename
	end

	next_filename = filename_of_now()
	res["FileID"] = next_filename
	#save_data(next_point, data, res, next_filename, last_filename)
	saving_data = res.clone
	save_data(saving_data)
	last_filename = next_filename

	result = res["returnValue"]
	data = res["stateData"]
	res["data"] = data

	if result == 0
		# show images
		begin
			next_point = plot_and_interact(res["imageList"])
			next_point = point_to_lambda(next_point[0], next_point[1])
		rescue SaveAs => e
			save_data(res, e.to_s)
			retry
		rescue HistoryPrev
			if history.empty?
				$stderr.puts "there is no previous history"
				retry
			end
			next_state = history.pop
			future.push res.clone
			res = next_state
			$stderr.puts "CurrentID: #{res["FileID"]}"
			retry
		rescue HistoryNext
			if future.empty?
				$stderr.puts "there is no next history"
				retry
			end
			next_state = future.pop
			history.push res.clone
			res = next_state
			$stderr.puts "CurrentID: #{res["FileID"]}"
			retry
		end

		history << res.clone
		future = []

		res["clickHistory"] = res["clickHistory"] + [next_point]
	else
		# interact with galaxy
		$stderr.puts "Interacting with Galaxy..."
		send_data = res["imageListAsData"] # "ap ap cons 0 nil"
		send_data = "ap ap cons 1113939892088752268 nil" if $a
		$a = true
		$stderr.puts "send_data: #{send_data}"
		send_data = modulate(send_data)
		$stderr.puts "modulated: #{send_data}"
		response = `curl -X POST "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=9ffa61129e0c45378b01b0817117622c" -H "accept: */*" -H "Content-Type: text/plain" -d "#{send_data}"`
		# $stderr.puts "Response From Galaxy: #{response}"
		next_point = demodulate(response)
		$stderr.puts "Next Point: #{next_point}"
	end
	res["point"] = next_point
	$stderr.puts data

	state = res
end
