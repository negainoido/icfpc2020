require 'json'
require 'pp'
require 'stringio'

def ppm_from_images(images)
	images.select!{|x| !x.empty?}

	xmin = images.flatten(1).map {|x, y| x}.min
	xmax = images.flatten(1).map {|x, y| x}.max
	ymin = images.flatten(1).map {|x, y| y}.min
	ymax = images.flatten(1).map {|x, y| y}.max

	pixels = Hash.new([0, 0, 0])

	images.each do |image|
		rgb = [255, 255, 255]
		opaq = 1.0 / images.length
		image.each do |x, y|
			pixels[ [x, y] ] = rgb.map {|c| (c * opaq).to_i}
		end
	end

	width = xmax - xmin + 1
	height = ymax - ymin + 1
	io = StringIO.new
	io.puts "P3"
	io.puts "#{width} #{height}"
	io.puts "255"
	for y in ymin..ymax
		for x in xmin..xmax
			c = pixels[[x,y]]
			io.puts c.join(" ")
		end
	end
	io.rewind
	io.read
end

if $0 == __FILE__
	json = JSON.load($stdin.read)
	images = json["imageList"]
	if !images
		$stderr.puts "Bad JSON file: no 'imageList' key"
		exit 1
	end
	puts ppm_from_images(images)
end

