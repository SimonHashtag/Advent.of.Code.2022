mutable struct Monkey
    id::Int64
    inspections::Int64
    worry::Vector{BigInt}
    operation::Function
    throw::Function

    function Monkey(
        id::Int64,
        inspections::Int64, 
        worry::Vector{BigInt}, 
        operation::Function, 
        throw::Function)
        new(id, inspections, worry, operation, throw)
    end
end

function get_operation(old_val::BigInt, txt_fun::String, divide_by::Int)
    op_parts = split(split(split(txt_fun, ":")[2], "= ")[2], " ")
    if op_parts[2] == "+"
        if op_parts[3] == "old"
            return(big(floor(BigInt, 2*old_val/divide_by)))
        else
            return(big(floor(BigInt, (old_val + parse(Int64, op_parts[3]))/divide_by)))
        end
    else
        if op_parts[3] == "old"
            return(big(floor(BigInt, (old_val^2)/divide_by)))
        else
            return(big(floor(BigInt, old_val * parse(Int64, op_parts[3])/divide_by)))
        end
    end
end

function throw_to(worry::BigInt, denominator_txt::String, true_command::String, false_command::String)
    denominator = parse(Int64, split(denominator_txt, "by ")[2])
    next_monkey_true = parse(Int, split(true_command, "monkey ")[2])
    next_monkey_false = parse(Int, split(false_command, "monkey ")[2])
    if worry % denominator == 0
        return(next_monkey_true)
    else
        return(next_monkey_false)
    end
end

function read_single_monkey(file_location::String)
    open(file_location) do file
        lines = readlines(file)
        return(i->lines[i:i+5], length(lines))
    end
end

function read_all_monkeys(file_location::String, divide_by::Int)
    get_monkey, lines_count = read_single_monkey(file_location)
    m = []
    monkey = []
    for i in 1:7:lines_count
        monkey = get_monkey(i)
        id = parse(Int, split(split(monkey[1], " ")[2], ":")[1])
        init_worry = parse.(BigInt, split(split(monkey[2], ":")[2], ","))
        push!(m, deepcopy(Monkey(
            id, 
            0,
            init_worry, 
            x -> get_operation(x, monkey[3], divide_by), 
            y -> throw_to(y, monkey[4], monkey[5], monkey[6])
            )))
    end
    return m
end

function run_round(monkeys)
    for monkey in monkeys
        for item in monkey.worry
            monkey.inspections += 1
            new_item = monkey.operation(item)
            recipient = monkey.throw(new_item)
            push!(monkeys[recipient+1].worry, new_item)
        end
        monkey.worry = []
    end
end

function get_monkey_business(monkeys, rounds=20)
    for i in 1:rounds
        run_round(monkeys)
    end
    inspection_list = []
    for monkey in monkeys
        push!(inspection_list, monkey.inspections)
    end
    sort!(inspection_list)
    println(inspection_list)
    return(inspection_list[end-1]*inspection_list[end])
end

function main()
    loc = "./input/day11.txt"
    monkeys = read_all_monkeys(loc, 1)
    bizz = get_monkey_business(monkeys, 200)
    return bizz
end

bizz = main()