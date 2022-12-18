function read_data(file_location)
    open(file_location) do file
        cycle_values = Vector{Int}()
        current_value = 1

        function noop_cycle()
            push!(cycle_values, current_value)
        end

        while !eof(file)
            x = readline(file)
            if startswith("noop", x)
                noop_cycle()
            else 
                noop_cycle()
                noop_cycle()
                current_value += parse(Int, x[6:end])
            end
        end
        return(cycle_values)
    end
end

function sum_signals(schedule, cycle_list)
    signal_sum = 0
    for cycle in cycle_list
        signal_sum += schedule[cycle] * cycle
    end
    return(signal_sum)
end

function run_CPR(schedule, vertical_pixel = 6, horizontal_pixel=40)
    for row in 1:vertical_pixel
        for col in 1:horizontal_pixel
            cpr_position = (row - 1) * 40 + col
            if col-1 in (schedule[cpr_position]-1:schedule[cpr_position]+1)
                print("#")
            else
                print(".")
            end
        end
        println("")
    end
end

function main()
    loc = "./input/day10.txt"
    schedule = read_data(loc)
    sig_sum = sum_signals(schedule, [20, 60, 100, 140, 180, 220])
    println("Sum of signals: ", sig_sum)
    run_CPR(schedule)
end

main()