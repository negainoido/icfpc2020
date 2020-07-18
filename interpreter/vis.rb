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
 
autotaker = IO.popen(["cabal", "new-exec", "interpreter"], "r+")
autotaker.puts File.open("galaxy.txt").read()
autotaker.close_write

#lines = system("cabal new-exec interpreter  <galaxy.txt")
lines = autotaker.read()
autotaker.close
#lines = File.open("data").read()
puts lines
dataascode = lines =~ /DataAsCode: (.*)/ ? $1 : nil
images = read_image_from_string(lines)

#plot = IO.popen("gnuplot", "r+")
plot = IO.popen("gnuplot", "r+", :err => [:child, :out])

images.length.times do |i|
	plot.puts "$image#{i} << EOD"
	images[i].each do |x, y|
		plot.puts "#{x} #{y}"
	end
	plot.puts "EOD"
end

data = images.length.times.map {|i| "$image#{i}"}.join(",")
plot.puts "plot #{data}"


plot.puts "set mouse verbose"

while l = plot.gets
	puts "gnuplot: #{l}"
	if l =~ /put `\s*(-?[-0-9\.]*),\s*(-?[0-9\.]*)' to clipboard\./
		x = $1.to_f
		y = $2.to_f
		p [x, y]
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

