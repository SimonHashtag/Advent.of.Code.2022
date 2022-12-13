read_data = function(file_location)
    input = open(file_location) do file
        input = []
        first_comp = Vector{String}()
        second_comp = Vector{String}()
        while !eof(file)
            x = readline(file)
            push!(first_comp, x[1:Int(length(x)/2)])
            push!(second_comp, x[Int(length(x)/2+1):Int(length(x))])
        end
        input = hcat(first_comp, second_comp)
    end
    return(input)
end

check_double = function(item_type, second_comp)
    return(occursin(item_type, second_comp))
end

get_sum = function(input, alphabet)
    priority_sum=0
    for row in eachrow(input)
        for character in row[1]
            if check_double(character, row[2]) == true
                priority_sum += indexin(character, alphabet)[1]
                break
            end
        end
    end
    return priority_sum
end

get_badge = function(input)
    badges = ""
    for row=1:3:size(input)[1]
        double_chars = ""
        for character in input[row]
            if check_double(character, input[row+1])
                double_chars *= character
            end
        end
        for letter in double_chars
            if check_double(letter, input[row+2])
                badges *= letter
                break
            end
        end
    end
    return(badges)
end

get_badge_priority_sum = function(input, alphabet)
    priority_sum=0
    badges = get_badge(input)
    for character in badges
        priority_sum += indexin(character, alphabet)[1]
    end
    return(priority_sum)
end


function main()
    loc = "./input/day3.txt"
    data = read_data(loc)
    alphabet = vcat(collect('a':'z'), collect('A':'Z'))
    prio_sum = get_sum(data, alphabet)

    # Second challenge
    input = [join(data[i,:]) for i in 1:size(data)[1]]
    prio_sum_2= get_badge_priority_sum(input, alphabet)
    return prio_sum, prio_sum_2
end

main()
