read_data = function(file_location)
    open(file_location) do file
        input = []
        while !eof(file)
            x = readline(file)
            push!(input, [parse(Int, m.match) for m in eachmatch(r"\d+", x)])
        end
        return(input)
    end
end

move_crate = function(stack_input, recipe, CM9000=true)
    input = deepcopy(stack_input)
    for row in eachrow(recipe)
        rule = row[1]
        if CM9000
            append!(input[rule[3]], reverse(input[rule[2]][end-rule[1]+1:end]))
        else
            append!(input[rule[3]], input[rule[2]][end-rule[1]+1:end])
        end
        deleteat!(input[rule[2]], size(input[rule[2]])[1]-rule[1]+1:size(input[rule[2]])[1])
    end
    return(input)
end

get_top_item = function(stack_input)
    input = deepcopy(stack_input)
    for row in input
        print(pop!(row))
    end
end


function main()
    stack_input = [
        reverse(["T", "Z", "B"]), 
        reverse(["N", "D", "T", "H", "V"]), 
        reverse(["D", "M", "F", "B"]), 
        reverse(["L", "Q", "V", "W", "G", "J", "T"]), 
        reverse(["M", "Q", "F", "V", "P", "G", "D", "W"]),
        reverse(["S", "F", "H", "G", "Q", "Z", "V"]),
        reverse(["W", "C", "T", "L", "R", "N", "S", "Z"]),
        reverse(["M", "R", "N", "J", "D", "W", "H", "Z"]),
        reverse(["S", "D", "F", "L", "Q", "M"])
        ]
    loc = "./input/day5.txt"
    data = read_data(loc)

    # CrateMover 9000
    println("CrateMover 9000: ")
    stack_output = move_crate(stack_input, data, true)
    get_top_item(stack_output)

    # CrateMover 9001
    println("\n\nCrateMover 9001: ")
    stack_output_CM9001 = move_crate(stack_input, data, false)
    get_top_item(stack_output_CM9001)
end

main()