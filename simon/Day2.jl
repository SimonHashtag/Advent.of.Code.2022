using Combinatorics

read_data = function(file_location)
    input = open(file_location) do file
        input = []
        elf = Vector{Char}()
        me = Vector{Char}()
        line = 1
        while !eof(file)
            x = readline(file)
            push!(elf, x[1])
            push!(me, x[3])
            line += 1
        end
        input = hcat(elf, me)
    end
    return(input)
end

build_scoreboard = function(shape_score_me) 
    score_board = vcat(reshape([hcat.(v...) for v in Iterators.product(["A", "B", "C"], ["X", "Y", "Z"])], 9, 1)...)
    shape_score_elf = Dict([("A", 1), ("B", 2), ("C", 3)])
    draw = Dict([("X", 1), ("Y", 2), ("Z", 3)])
    
    total_score = []
    for row in eachrow(score_board)
        if (row[1]=="A" && row[2]=="Z") || (row[1]=="B" && row[2]=="X") || (row[1]=="C" && row[2]=="Y")
            win_score = 0
        elseif shape_score_elf[row[1]]==draw[row[2]]
            win_score = 3
        else
            win_score = 6
        end
        push!(total_score, shape_score_me[row[2]]+win_score)
    end
    score_board = hcat(score_board, total_score)
    return(score_board)
end

determine_score = function(data, score_board)
    score = 0
    for row in eachrow(data)
        score += score_board[findall((score_board[:, 1] .== string(row[1])) .& (score_board[:, 2] .== string(row[2])))[1], 3]
    end
    return score
end

get_one_choice = function(row, win_board, score_board, desired_win_score)
    strategy = win_board[findall((win_board[:, 1] .== string(row[1])) .& (win_board[:, 3] .== desired_win_score)), 2]
    score = score_board[findall((score_board[:, 1] .== string(row[1])) .& (score_board[:, 2] .== string(strategy[1])))[1], 3]
    return(score)
end

get_secret_strategy_score = function(data, score_board)
    zero_shape_score = Dict([("X", 0), ("Y", 0), ("Z", 0)])
    win_board = build_scoreboard(zero_shape_score)
    score=0
    for row in eachrow(data)
        if string(row[2])=="X"
            single_score = get_one_choice(row, win_board, score_board, 0)
        elseif string(row[2])=="Y"
            single_score = get_one_choice(row, win_board, score_board, 3)
        else
            single_score = get_one_choice(row, win_board, score_board, 6)
        end
        score += single_score
    end
    return(score)
end

function main()
    loc = "./input/day2.txt"
    data = read_data(loc)
    shape_score_me = Dict([("X", 1), ("Y", 2), ("Z", 3)])
    score_board = build_scoreboard(shape_score_me)
    total_score = determine_score(data, score_board)
    new_total_score = get_secret_strategy_score(data, score_board)
    return total_score, new_total_score
end

main()




