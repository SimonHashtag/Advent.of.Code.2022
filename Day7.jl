using AbstractTrees

mutable struct DirNode{T}
    data::T
    parent::Union{Nothing, DirNode{T}}
    children::Vector{Union{Nothing, Int, DirNode{T}}}
    value::Union{Nothing, Int}

    function DirNode{T}(data, parent=nothing, children=[], value=0) where T
        new{T}(data, parent, children, value)
    end
end;
DirNode(data) = DirNode{typeof(data)}(data)

function addchild!(parent::DirNode, data)
    node = typeof(parent)(data, parent)
    push!(parent.children, node)
end

function addleaf!(parent::DirNode, data)
    push!(parent.children, data)
end

AbstractTrees.children(n::DirNode) = n.children
AbstractTrees.nodevalue(n::DirNode) = n.data
AbstractTrees.parent(n::DirNode) = n.parent
AbstractTrees.ParentLinks(::Type{<:DirNode}) = StoredParents()

function get_child(parent_node::DirNode, search_value)
    for i in children(parent_node)
        if nodevalue(i) == search_value
            return i
        end
    end
end

function add_values(parent)
    if typeof(parent) == Int 
        return(parent)
    elseif (parent.value != 0)
        println(child.value)
        return(parent.value)
    else
        for child in children(parent)
            global parent.value += add_values(child)
        end
    return(parent.value)
    end
end

function get_sum_small_dirs(root_node, max_size)
    total_sum = 0
    for node in PostOrderDFS(root_node)
        if (typeof(node) == DirNode{String} && node.value < max_size) total_sum += node.value end
    end
    return(total_sum)
end

function get_deleted_dir(root_node, min_size)
    dir_size = root_node.value
    for node in PostOrderDFS(root_node)
        if (typeof(node) == DirNode{String} && node.value > min_size) 
            dir_size = min(dir_size, node.value) 
        end
    end
    return(dir_size)
end

grow_tree = function(x, parent)
    if startswith(x, r"\$ cd [a-z]")
        parent = get_child(parent, x[6: end])
    elseif startswith(x, r"\$ cd \.")
        parent = parent.parent
    elseif startswith(x, "dir")
        addchild!(parent, x[5:end])
    elseif startswith(x, r"\d+")
        addleaf!(parent, [parse(Int, m.match) for m in eachmatch(r"\d+", x)][1])
    end
    return parent
end

get_root = function(node)
    while !isroot(node)
        node = node.parent
    end
    return(node)
end

read_data = function(file_location)
    open(file_location) do file
        parent = DirNode("/")
        while !eof(file)
            x = readline(file)
            parent = grow_tree(x, parent)
        end
        tree = get_root(parent)
        return(tree)
    end
end

function main()
    loc = "input/day7.txt"
    tree = read_data(loc)
    add_values(tree)
    t_sum = get_sum_small_dirs(tree, 100000)
    storage_del_dir = get_deleted_dir(tree, 8381165)

    #print_tree(tree)
    println(t_sum)
    println(storage_del_dir)
end

main()