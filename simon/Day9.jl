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
    elseif coord_1 == coord_2
        return true
    end
    return false
end

function get_surface(coord_1, coord_2)
    abs(coord_1[1] - coord_2[1]) * abs(coord_1[2] - coord_2[2])
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
    if dir == 'L'
        if get_surface(leader_pos, follower_pos) > 1
            follower_pos[2] = leader_pos[2]
            follower_pos[1] -= 1
        elseif !is_touching(leader_pos, follower_pos)
            follower_pos[1] -= 1
        end
    elseif dir == 'R'
        if get_surface(leader_pos, follower_pos) > 1
            follower_pos[2] = leader_pos[2]
            follower_pos[1] += 1
        elseif !is_touching(leader_pos, follower_pos)
            follower_pos[1] += 1
        end
    elseif dir == 'D'
        if get_surface(leader_pos, follower_pos) > 1
            follower_pos[1] = leader_pos[1]
            follower_pos[2] -= 1
        elseif !is_touching(leader_pos, follower_pos)
            follower_pos[2] -= 1
        end
    else
        if get_surface(leader_pos, follower_pos) > 1
            follower_pos[1] = leader_pos[1]
            follower_pos[2] += 1
        elseif !is_touching(leader_pos, follower_pos)
            follower_pos[2] += 1
        end
    end
    return(follower_pos)
end

function model_rope(recipe)
    position_head = zeros(2)
    tail_knots = [zeros(2) for _ in 1:9]
    position_tail = zeros(2)
    tail_pos_array = []
    for row in eachrow(recipe)
        for _ in 1:row[2]
            position_head = get_leader_position(row[1], position_head)
            tail_knots[1] = get_follower_position(row[1], position_head, position_tail)
            for i in 2:length(tail_knots)
                tail_knots[i] = get_follower_position(row[1], tail_knots[i-1], tail_knots[i])
            end
            push!(tail_pos_array, deepcopy(tail_knots[end]))
        end
    end
    return(tail_pos_array)
end

function get_row_length(recipe)
    side_ways = recipe[(recipe[:, 1] .=='L') .| (recipe[:, 1] .=='R'),:]
    position = 0
    left_border = 0
    right_border = 0
    for row in eachrow(side_ways)
        if row[1] == 'L'
            position -= row[2]
            left_border = min(left_border, position)
        else
            position += row[2]
            right_border = max(right_border, position)
        end
    end
    return(right_border - left_border)
end

function main()
    loc = "./input/day9_debug.txt"
    recipe = read_data(loc)
    positions = model_rope(recipe)
    println(size(unique(positions))[1])
    return positions
end

main()