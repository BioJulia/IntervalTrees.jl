
module TestIntervalTrees

using FactCheck
using IntervalTrees


# Generating random intervals
srand(12345)
const maxend = int(1e9)

function randinterval(minstart=1, maxend=maxend)
    a = rand(minstart:maxend)
    b = rand(a:maxend)
    return (a, b)
end


# Verify that internal node keys and maxend values are correct
function validkeys(t::IntervalTrees.IntervalBTree)
    return validkeys(t.root, (0, 0), (maxend, maxend), maxend)
end


function validkeys(node::IntervalTrees.InternalNode, minint, maxint, maxend)
    for nodemaxend in node.maxends
        if nodemaxend > maxend
            return false
        end
    end

    for i in 1:length(node.keys)
        k = node.keys[i]
        if !validkeys(node.children[i], minint, k, node.maxends[i]) ||
           !validkeys(node.children[i+1], k, maxint, node.maxends[i+1])
           return false
       end
    end
    return true
end


function validkeys(node::IntervalTrees.LeafNode, minint, maxint, maxend)
    for k in node.keys
        if !(minint <= k < maxint) || k[2] > maxend
            println(STDERR, (k[2], maxend))
            return false
        end
    end
    return true
end


# Verify that parent pointers are correct
function validparents(t::IntervalTrees.IntervalBTree)
    if !isa(t.root.parent, IntervalTrees.NullNode)
        return false
    end

    return validparents(t.root)
end


function validparents(node::IntervalTrees.InternalNode)
    for child in node.children
        if !validparents(child) || child.parent != node
            return false
        end
    end
    return true
end

function validparents(node::IntervalTrees.LeafNode)
    return true
end


# Verify that sibling/cousin pointers are correct
function validsiblings(t::IntervalTrees.IntervalBTree)
    if !isa(t.root.left, IntervalTrees.NullNode) ||
       !isa(t.root.right, IntervalTrees.NullNode)
       return false
   end

   # Do an in-order traversal, pushing nodes onto a stack indexed by
   # depth.
   tdepth = depth(t)
   nodestacks = [Array(IntervalTrees.Node, 0) for _ in 1:tdepth]

   function _visit(node::IntervalTrees.InternalNode, k)
       push!(nodestacks[k], node)
       for child in node.children
           _visit(child, k+1)
       end
   end

   function _visit(node::IntervalTrees.LeafNode, k)
       push!(nodestacks[k], node)
   end

   _visit(t.root, 1)

   for k in 1:tdepth
       stack = nodestacks[k]
       if isempty(stack)
           continue
       end

       if !isa(stack[1].left, IntervalTrees.NullNode) ||
          !isa(stack[end].right, IntervalTrees.NullNode)
           return false
       end

       if length(stack) > 1
           if stack[1].right != stack[2] || stack[end].left != stack[end-1]
               return false
           end
       end

       for i in 2:length(stack)-1
           if stack[i].left != stack[i-1] || stack[i].right != stack[i+1]
               return false
           end
       end
   end

   return true
end


facts("Searching") do
    t = IntervalTree{Int, Int}()
    @fact haskey(t, (1,2)) => false

    n = 10000
    maxend = 1000000
    intervals = [randinterval(1, maxend) for i in 1:n]

    context("true positives") do
        for (i, interval) in enumerate(intervals)
            t[interval] = i
        end

        shuffle!(intervals)
        @fact all([haskey(t, interval) for interval in intervals]) => true
    end

    context("true negatives") do
        @fact all([!haskey(t, interval)
                   for interval in [randinterval(maxend+1, 2 * maxend)
                                    for i in 1:n]]) => true
    end
end


facts("Intersecting") do
    t = IntervalTree{Int, Int}()

end


facts("Iteration") do
    t = IntervalTree{Int, Int}()
    @fact isempty(collect(t)) => true
end


facts("Insertion") do
    t = IntervalTrees.IntervalTree{Int, Int}()
    n = 100000

    context("random insertions") do
        for v in 1:n
            k = randinterval()
            t[k] = v
        end
        @fact issorted(collect(keys(t))) => true
        @fact validkeys(t) => true
        @fact validparents(t) => true
        @fact validsiblings(t) => true
    end

    context("ordered insertions") do
        for v in 1:n
            k = (v,rand(v:maxend))
            t[k] = v
        end
        @fact issorted(collect(keys(t))) => true
        @fact validkeys(t) => true
        @fact validparents(t) => true
        @fact validsiblings(t) => true
    end

    context("reverse ordered insertions") do
        for v in 1:n
            k = (maxend-v,rand((maxend-v):maxend))
            t[k] = v
        end
        @fact issorted(collect(keys(t))) => true
        @fact validkeys(t) => true
        @fact validparents(t) => true
        @fact validsiblings(t) => true
    end
end


facts("Deletion") do
    t = IntervalTrees.IntervalTree{Int, Int}()
    n = 100000

    # insert n random intervals
    intervals = [randinterval() for _ in 1:n]
    for (i, interval) in enumerate(intervals)
        t[interval] = i
    end
    shuffle!(intervals)

    # delete one
    interval = pop!(intervals)
    delete!(t, interval)
    @fact validkeys(t) => true
    @fact validparents(t) => true
    @fact validsiblings(t) => true
    @fact issorted(collect(keys(t))) => true
    @fact haskey(t, interval) => false
    @fact all(map(interval -> haskey(t, interval), intervals)) => true

    # delete a random 50%
    for interval in 1:(n-1)/2
        interval = pop!(intervals)
        delete!(t, interval)
    end
    @fact validkeys(t) => true
    @fact validparents(t) => true
    @fact validsiblings(t) => true
    @fact issorted(collect(keys(t))) => true
    @fact all(map(interval -> haskey(t, interval), intervals)) => true

    # delete the rest
    for interval in intervals
        delete!(t, interval)
    end
    @fact isempty(t) => true
    @fact validkeys(t) => true
    @fact validparents(t) => true
    @fact validsiblings(t) => true
end


end

