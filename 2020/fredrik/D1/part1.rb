list = File.read("data.txt").split

list.each do |i|
    list.each do |j|
        if i.to_i + j.to_i == 2020
            puts "#{i} #{j} #{i.to_i*j.to_i}"
        end
    end
end