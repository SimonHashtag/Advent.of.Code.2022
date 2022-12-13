using AbstractTrees

#### Struct and functions for building a directory tree ####

mutable struct DirNode{T}
    data::T
    parent::Union{Nothing, DirNode{T}}
    children::Vector{Union{Nothing, Int, DirNode{T}}}
    value::Union{Nothing, Int}

    # Init function for node
    function DirNode{T}(data, parent=nothing, children=[], value=0) where T
        new{T}(data, parent, children, value)
    end
end;

DirNode(data) = DirNode{typeof(data)}(data)

# Interface functions
AbstractTrees.children(n::DirNode) = n.children  # get children nodes
AbstractTrees.nodevalue(n::DirNode) = n.data  # get name of directory
AbstractTrees.parent(n::DirNode) = n.parent # get parent node

# Children (type: Dirnode) are stored in a vector
function addchild!(parent::DirNode, data)
    node = typeof(parent)(data, parent)
    push!(parent.children, node)
end

# Leaves are Integers (file size)
function addleaf!(parent::DirNode, data)
    push!(parent.children, data)
end

# Find a child given its name
function get_child(parent_node::DirNode, search_value)
    for i in children(parent_node)
        if nodevalue(i) == search_value
            return i
        end
    end
end

# Build directory tree given a terminal output
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

# Recursively add dir sizes
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

#### Evaluate directory tree ####

# Get the sum of all dirs that are smaller than max_size descending from root_node
function get_sum_small_dirs(root_node, max_size)
    total_sum = 0
    for node in PostOrderDFS(root_node)
        if (typeof(node) == DirNode{String} && node.value < max_size) total_sum += node.value end
    end
    return(total_sum)
end

# Get smallest directory that has min_size size
function get_deleted_dir(root_node, min_size)
    dir_size = root_node.value
    for node in PreOrderDFS(root_node)
        if (typeof(node) == DirNode{String} && node.value >= min_size && node.value < dir_size)
            dir_size = node.value
        end
    end
    return(dir_size)
end

# Get root of tree
get_root = function(node)
    while !isroot(node)
        node = node.parent
    end
    return(node)
end

# Read terminal output and build a dir tree
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
    loc = "./input/day7.txt"
    tree = read_data(loc)
    add_values(tree)
    t_sum = get_sum_small_dirs(tree, 100000)
    storage_del_dir = get_deleted_dir(tree, tree.value - 40000000)

    print_tree(tree)
    println("Total sum of sizes of small directories: " * string(t_sum))
    println("Size of directory to be deleted: " * string(storage_del_dir))
end

main()