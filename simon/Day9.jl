# Uncomment for plotting the rope in realtime
#using Gaston
using Plots

read_data = function(file_location)
    open(file_location) do file
        input = []
        direction = Vector{Char}()
        steps = Vector{Int}()
        while !eof(file)
            x = readline(file)
            push!(direction, x[1])
            push!(steps, parse(Int, x[3:end]))
        end
        input = hcat(direction, steps)
        return(input)
    end
end

function get_distance(coord_1, coord_2)
    abs(coord_1[1] - coord_2[1]) + abs(coord_1[2] - coord_2[2])
end

function is_touching(coord_1, coord_2)
    if get_distance(coord_1, coord_2) <= 1
        return true
    elseif get_distance(coord_1, coord_2) == 2 && ((coord_1[1] != coord_2[1]) && (coord_1[2] != coord_2[2]))
        return true
    end
    return false
end

function get_leader_position(dir, leader_pos)
    if dir == 'L'
        leader_pos[1] -= 1
    elseif dir == 'R'
        leader_pos[1] += 1
    elseif dir == 'D'
        leader_pos[2] -= 1
    else
        leader_pos[2] += 1
    end
    return(leader_pos)
end

function get_follower_position(dir, leader_pos, follower_pos)
    if is_touching(leader_pos, follower_pos)
        return(follower_pos)
    end
    follower_pos[1] += 1 * (leader_pos[1] > follower_pos[1]) - 1 * (leader_pos[1] < follower_pos[1])
    follower_pos[2] += 1 * (leader_pos[2] > follower_pos[2]) - 1 * (leader_pos[2] < follower_pos[2])

    return(follower_pos)
end

function model_rope(recipe, no_knots)
    position_head = zeros(2)
    tail_knots = [zeros(2) for _ in 1:no_knots-1]
    position_tail = zeros(2)
    tail_pos_array = []
    # evo = plot(reduce(hcat, tail_knots)[1,:], reduce(hcat, tail_knots)[2,:])
    for row in eachrow(recipe)
        for _ in 1:row[2]
            position_head = get_leader_position(row[1], position_head)
            tail_knots[1] = get_follower_position(row[1], position_head, position_tail)
            for i in 2:no_knots-1
                tail_knots[i] = get_follower_position(row[1], tail_knots[i-1], tail_knots[i])
            end
            push!(tail_pos_array, deepcopy(tail_knots[end]))

            #= current_plot = plot(reduce(hcat, [zeros(2) for _ in 1:no_knots-1])[1,:], reduce(hcat, [zeros(2) for _ in 1:no_knots-1])[2,:], handle=2);
            plot!(reduce(hcat, tail_knots)[1,:], reduce(hcat, tail_knots)[2,:],  handle=2);
            push!(evo, current_plot) =#

        end
    end
    return(tail_pos_array)
end

function main()
    loc = "./input/day9.txt"
    recipe = read_data(loc)
    positions = model_rope(recipe, 10)
    display(scatter(reduce(hcat, positions)[1,:], reduce(hcat, positions)[2,:]))
    println(size(unique(positions))[1])
    return positions
end

main()