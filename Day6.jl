read_data = function(file_location)
    input = open(file_location) do file
        read(file,String)
    end
    return(input)
end

reset_count = function(buffer, index, end_search_range) 

end

get_number_of_processes = function(buffer, marker_length)
    unique_count = 0
    for i in eachindex(buffer)
        unique_count += 1
        if (i+marker_length-unique_count)>=length(buffer)
            if contains(buffer[i-unique_count+1:end], buffer[i])
                unique_count = 0
            end
        else
            if contains(buffer[i+1:(i+marker_length-unique_count)], buffer[i])
                unique_count = 0
            end
        end
        if unique_count == marker_length
            return(i)
        end
    end
end

function main()
    loc = "input/day6.txt"
    buffer = read_data(loc)
    start_of_packet_marker_no = get_number_of_processes(buffer, 4)
    start_of_message_marker_no = get_number_of_processes(buffer[1:end-1], 14)
end

main()