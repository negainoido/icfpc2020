#!/usr/bin/env ruby
require 'json'
require 'pp'
require 'fileutils'
require 'stringio'
require 'open3'
require_relative 'image'

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



require 'optparse'
opt = OptionParser.new

$options = {}
opt.on('-f', '--file FILENAME', 'load from FILENAME') {|v| $options[:file] = v}

opt.parse!(ARGV)

stdin = $stdin
if $options[:file]
	stdin = File.open($options[:file])
end


@plot = IO.popen("gnuplot", "r+", :err => [:child, :out])

while line = stdin.gets
	case line
	when /\Astate:\s*(.*)/
		state = JSON.parse($1)
		@plot.puts Image.new(state).to_s
		sleep 0.5
	end
end
