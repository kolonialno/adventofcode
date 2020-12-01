list = File.read("data.txt").split

list.each do |i|
    list.each do |j|
        list.each do |k|
            if i.to_i + j.to_i + k.to_i == 2020
                puts "#{i} #{j} #{k} #{i.to_i*j.to_i*k.to_i}"
            end
        end
    end
end