read_data = function(file_location)
    input = open(file_location) do file
        read(file,String)
    end
    
    findnext("1", input, 2)
    occursin(r"\r\n\r\n", input)
    
    input = replace(input, r"\r\n\r\n" => ")(")
    input = replace(input, r"\r\n" => ",")
    input = string(input[1:length(input)-2], ")")
    return(input)
end

txt_to_array = function(input)
    previous = 1
    d = Set()
    output = Int64[]
    next=0
    while true
        next = findnext(r",|\)", input, previous)[1]
        push!(output, parse(Int64,input[previous:next-1]))
        if next == length(input)
            push!(d, output)
            break
        end
        if findnext(r",", input, previous)[1] > findnext(r"\)", input, previous)[1]
            push!(d, output)
            output = Int64[]
            previous = next + 2
        else 
            previous = next+1
        end
    end
    return d
end

get_top = function(data_set, no_top=3)
    sums = Int64[]
    for i in data_set
        push!(sums, sum(i))
    end
    top = sort(sums, rev=true)[1:no_top]
    sum_top = sum(top)
    return(top, sum_top)
end

function main()
    loc = "input/day1.txt"
    no_elfs = 5
    txt_data = read_data(loc)
    elf_set = txt_to_array(txt_data)
    top = get_top(elf_set, no_elfs)
    println("Calories for top ", no_elfs, " elfs: ", top[1], ". Sum of these calories: ", top[2])
end

main()