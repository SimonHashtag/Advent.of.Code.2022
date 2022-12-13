## Read txt file and make matrix
read_data = function(file_location)
    open(file_location) do file
        tree_array = []
        while !eof(file)
            x = readline(file)
            row = string_to_vector(x)
            push!(tree_array, row)
        end
        return(mapreduce(permutedims, vcat, tree_array))
    end
end

function string_to_vector(input, delimiter="")
    return([parse(Int, digit) for digit in split(input, delimiter)])
end


## Check whether tree is visible (no larger tree right, left, up, or down)
function is_visible(tree_map, row_index, col_index)
    tree_size = tree_map[row_index, col_index]
    !(any(tree_map[1:row_index-1, col_index] .>= tree_size) && 
    any(tree_map[row_index+1:end, col_index] .>= tree_size) && 
    any(tree_map[row_index, 1:col_index-1] .>= tree_size) && 
    any(tree_map[row_index, col_index+1:end] .>= tree_size))
end

function count_visible(tree_map)
    tree_count = 0
    for row in 2:size(tree_map)[1]-1
        for col in 2:size(tree_map)[2]-1
            tree_count += is_visible(tree_map, row, col)
        end
    end
    tree_count += 2*size(tree_map)[1] + 2*size(tree_map)[2] - 4
    return(tree_count)
end

## Helper function for get_tree_score (=> findfirst never returns nothing)
function grow_hedge(jungle)
    garden = deepcopy(jungle)
    garden[1, :] .= 9
    garden[end, :] .= 9
    garden[:, 1] .= 9
    garden[:, end] .= 9
    return(garden)
end

## Get scenic tree score for one tree
function get_tree_score(tree_map, row_index, col_index)
    garden_map = grow_hedge(tree_map)
    left_score = findfirst(garden_map[row_index, col_index-1:-1:1] .>= garden_map[row_index, col_index]) 
    right_score = findfirst(garden_map[row_index, col_index+1:end] .>= garden_map[row_index, col_index]) 
    up_score = findfirst(garden_map[row_index-1:-1:1, col_index] .>= garden_map[row_index, col_index]) 
    down_score = findfirst(garden_map[row_index+1:end, col_index] .>= garden_map[row_index, col_index]) 
    return(left_score * right_score * up_score * down_score)
end

function get_highest_scenic_score(tree_map)
    scenic_score = 0
    for row in 2:size(tree_map)[1]-1
        for col in 2:size(tree_map)[2]-1
            scenic_score = max(scenic_score, get_tree_score(tree_map, row, col))
        end
    end
    return(scenic_score)
end

function main()
    loc = "./input/day8.txt"
    tree_map = read_data(loc)
    tree_count = count_visible(tree_map)
    max_scenic_score = get_highest_scenic_score(tree_map)
    
    return tree_count, max_scenic_score
end

main()