def demodulate(s)
	prefix = s.shift + s.shift

	res = []
	case prefix
	when "00"
		res << "nil"
	when "11"
		res += demodulate(s)
		res += demodulate(s)
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
		num =  s[0..len-1].reverse.join("").to_i(2)
		res << sign * num
		len.times { s.shift }
	end
	res
end
a = "110110000111011111100001001111110101000000".split("")
#a = "1101000".split("")
p demodulate(a)
p a

a = "110110000111011111100001001111110100110000".split("")
p demodulate(a)
p a

p demodulate("110110000111011111100001001101111010100000".split(""))

p demodulate("110110000111011111100001001101010110011100".split(""))


p demodulate("110110000111011111100001001101010110011100".split(""))

