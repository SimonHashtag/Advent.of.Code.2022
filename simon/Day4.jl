read_data = function(file_location)
    input = open(file_location) do file
        input = []
        elf1 = Vector{String}()
        elf2 = Vector{String}()
        while !eof(file)
            x = readline(file)
            pair_break = findfirst(r",", x)[1]
            push!(elf1, x[1:pair_break-1])
            push!(elf2, x[pair_break+1:Int(length(x))])
        end
        input = hcat(elf1, elf2)
    end
    return(input)
end

get_start_end_task = function(task)
    delimiter = findfirst(r"-", task)[1] 
    return(parse(Int8, task[1:delimiter-1]), parse(Int8, task[delimiter+1:Int(length(task))]))
end

is_fully_contained = function(task1, task2)
    begin1, end1 = get_start_end_task(task1)
    begin2, end2 = get_start_end_task(task2)
    if begin1>begin2
        if end2 >= end1
            return(true)
        end
        return(false)
    elseif begin1<begin2
        if end1 >= end2
            return(true)
        end
        return(false)
    else begin1==begin2
        return(true)
    end
end

is_overlapped = function(task1, task2)
    begin1, end1 = get_start_end_task(task1)
    begin2, end2 = get_start_end_task(task2)

    if is_fully_contained(task1, task2)
        return(true)
    elseif (begin1>=begin2 && begin1 <=end2 || begin1<=begin2 && end1>=begin2)
        return(true)
    else 
        return(false)
    end
end

get_count_contained = function(input)
    count=0
    for row in eachrow(input)
        count += is_fully_contained(row[1], row[2])
    end
    return(count)
end

get_count_overlap = function(input)
    count = 0
    for row in eachrow(input)
        count += is_overlapped(row[1], row[2])
    end
    return(count)
end


function main()
    loc = "./input/day4.txt"
    data = read_data(loc)
    number_fully_contained = get_count_contained(data)
    number_overlapped = get_count_overlap(data)
    return(number_fully_contained, number_overlapped)
end

main()