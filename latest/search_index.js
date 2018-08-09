var documenterSearchIndex = {"docs": [

{
    "location": "index.html#",
    "page": "Home",
    "title": "Home",
    "category": "page",
    "text": "<h1><img align=\"right\" src=\"./sticker.svg\" width=\"30%\" /> IntervalTrees</h1>(Image: Latest Release) (Image: MIT License) (Image: Stable documentation) (Image: Lifecycle) (Image: Chat on Discord)"
},

{
    "location": "index.html#Description-1",
    "page": "Home",
    "title": "Description",
    "category": "section",
    "text": "IntervalTrees provides the type: IntervalTree{K, V}. It implements an associative container mapping (K, K) pairs to to values of type V. K may be any ordered type, but only pairs (a, b) where a ≤ b can be stored. In other words they are associative contains that map intervals to values."
},

{
    "location": "index.html#Installation-1",
    "page": "Home",
    "title": "Installation",
    "category": "section",
    "text": "Install IntervalTrees from the Julia REPL:using Pkg\nadd(\"IntervalTrees\")\n# Pkg.add(\"IntervalTrees\") for julia prior to v0.7If you are interested in the cutting edge of the development, please check out the master branch to try new features before release."
},

{
    "location": "index.html#Testing-1",
    "page": "Home",
    "title": "Testing",
    "category": "section",
    "text": "IntervalTrees is tested against Julia 0.7 on Linux, OS X, and Windows.PackageEvaluator Latest Build Status\n(Image: ) (Image: ) (Image: ) (Image: )"
},

{
    "location": "index.html#Contributing-1",
    "page": "Home",
    "title": "Contributing",
    "category": "section",
    "text": "We appreciate contributions from users including reporting bugs, fixing issues, improving performance and adding new features.Take a look at the CONTRIBUTING file provided with every BioJulia package package for detailed contributor and maintainer guidelines."
},

{
    "location": "index.html#Financial-contributions-1",
    "page": "Home",
    "title": "Financial contributions",
    "category": "section",
    "text": "We also welcome financial contributions in full transparency on our open collective. Anyone can file an expense. If the expense makes sense for the development of the community, it will be \"merged\" in the ledger of our open collective by the core contributors and the person who filed the expense will be reimbursed."
},

{
    "location": "index.html#Backers-and-Sponsors-1",
    "page": "Home",
    "title": "Backers & Sponsors",
    "category": "section",
    "text": "Thank you to all our backers and sponsors!Love our work and community? Become a backer.(Image: backers)Does your company use BioJulia? Help keep BioJulia feature rich and healthy by sponsoring the project Your logo will show up here with a link to your website.(Image: ) (Image: ) (Image: ) (Image: ) (Image: ) (Image: ) (Image: ) (Image: ) (Image: ) (Image: )"
},

{
    "location": "index.html#Questions?-1",
    "page": "Home",
    "title": "Questions?",
    "category": "section",
    "text": "If you have a question about contributing or using BioJulia software, come on over and chat to us on Discord, or you can try the Bio category of the Julia discourse site."
},

{
    "location": "man/types.html#",
    "page": "Types",
    "title": "Types",
    "category": "page",
    "text": ""
},

{
    "location": "man/types.html#Intervals-and-IntervalTrees-1",
    "page": "Types",
    "title": "Intervals & IntervalTrees",
    "category": "section",
    "text": "IntervalTrees exports an abstract type AbstractInterval{K}. Types deriving from it are expected to implement first and last methods that return the values of type K giving the inclusive range of the interval.There are also basic interval type provided:struct Interval{T} <: AbstractInterval{T}\n    first::T\n    last::T\nend\n\nstruct IntervalValue{K, V} <: AbstractInterval{K}\n    first::K\n    last::K\n    value::V\nendIntervals in this package are always treated as end-inclusive, similar to the Julia Range type.IntervalTrees exports one type: IntervalTree{K, V}.  It implements an associative container mapping (K, K) pairs to to values of type V.  K may be any ordered type, but only pairs (a, b) where a ≤ b can be stored."
},

{
    "location": "man/types.html#IntervalTrees-1",
    "page": "Types",
    "title": "IntervalTrees",
    "category": "section",
    "text": "The basic data structure implemented in this package is IntervalTree{K, V}, which stores intervals of type V, that have start and end positions of type K.IntervalMap{K, V} is a typealias for IntervalTree{K, IntervalValue{K, V}} to simplify associating data of type V with intervals.Multiple data structures are refered to as \"interval trees\". What\'s implemented here is the data structure described in the Cormen, et al. \"Algorithms\" book, or what\'s refered to as an augmented tree in the wikipedia article. This sort of data structure is just an balanced search tree augmented with a field to keep track of the maximum interval end point in that node\'s subtree.Many operations over two or more sets of intervals can be most efficiently implemented by jointly iterating over the sets in order. For example, finding all the intersecting intervals in two sets S and T can be implemented similarly to the merge function in mergesort in O(n+m) time.Thus a general purpose data structure should be optimized for fast in-order iteration while efficiently supporting other operations like insertion, deletion, and single intersection tests. A B+-tree is nicely suited to the task. Since all the intervals and values are stored in contiguous memory in the leaf nodes, and the leaf nodes augmented with sibling pointers, in-order traversal is exceedingly efficient compared to other balanced search trees, while other operations are comparable in performance."
},

{
    "location": "man/insertion.html#",
    "page": "Creating IntervalTrees",
    "title": "Creating IntervalTrees",
    "category": "page",
    "text": ""
},

{
    "location": "man/insertion.html#Creating-IntervalTrees-and-inserting-intervals-1",
    "page": "Creating IntervalTrees",
    "title": "Creating IntervalTrees & inserting intervals",
    "category": "section",
    "text": "New intervals can be added to an IntervalTree with the push! function.xs = IntervalTree{Int, Interval{Int}}()\npush!(xs, Interval{Int}(500, 1000))A more efficient means of building the data structure by bulk insertion. If the intervals are knows up front and provided in a sorted array, an IntervalTree can be built extremely efficiently.intervals = Interval{Int}[]\n# construct a large array of intervals...\n\nsort!(intervals)\nxs = IntervalTree{Int, Interval{Int}}(intervals)"
},

{
    "location": "man/dict.html#",
    "page": "Dictionary Operations",
    "title": "Dictionary Operations",
    "category": "page",
    "text": ""
},

{
    "location": "man/dict.html#Standard-Dictionary-Operations-1",
    "page": "Dictionary Operations",
    "title": "Standard Dictionary Operations",
    "category": "section",
    "text": "IntervalTree implements all the standard dictionary operations. You can use it as an efficient way to map (K, K) tuples to values.using IntervalTrees\n\n# Create an interval tree mapping (Int, Int) intervals to Strings.\nxs = IntervalMap{Int, String}()\n\n# Insert values\nxs[(1,100)] = \"Low\"\nxs[(101,1000)] = \"Medium\"\nxs[(1001,10000)] = \"High\"\n\n# Search for values\nprintln(xs[(1001,10000)]) # prints \"High\"\n\n# Get a value, returning a default value if not found\nprintln(get(xs, (10001, 100000), \"Not found\")) # prints \"Not found\"\n\n# Set a value if it\'s not already present\nprintln(set(xs, (10001, 100000), \"Not found\"))\n\n# Delete values\ndelete!(xs, (1,100))\n"
},

{
    "location": "man/iteration.html#",
    "page": "Iterating over intervals",
    "title": "Iterating over intervals",
    "category": "page",
    "text": ""
},

{
    "location": "man/iteration.html#Iteration-1",
    "page": "Iterating over intervals",
    "title": "Iteration",
    "category": "section",
    "text": "As with dictionaries, key/value pairs can be iterated through efficiently.    for x in xs\n        println(\"Interval $(x.first), $(x.last) has value $(x.value)\")\n    endSome other iteration functions are provided:from(t::IntervalTree, query): Return an iterator thats iterates through every key/value pair with an end position >= to query.keys(t::IntervalTree): Return an iterator that iterates through every interval key in the tree.values(t::IntervalTree): Return an iterator that iterates through every value in the tree."
},

{
    "location": "man/intersection.html#",
    "page": "Intersection",
    "title": "Intersection",
    "category": "page",
    "text": ""
},

{
    "location": "man/intersection.html#Intersection-1",
    "page": "Intersection",
    "title": "Intersection",
    "category": "section",
    "text": "The primary thing an IntervalTree offers over a Dict is the ability to efficiently find intersections. IntervalTrees supports searching and iterating over intersections between two trees or between a tree and a single interval.intersect(t::IntervalTree, query::(Any, Any)): Return an iterator over every interval in t that intersects query.intersect(t1::IntervalTree, t2::IntervalTree): Return an iterator over every pair of intersecting entries (interval1, interval2), where interval1 is in t1 and interval2 is in t2.hasintersection(t::IntervalTree, position): Return true if position intersects some interval in t."
},

]}
