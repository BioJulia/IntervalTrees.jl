using Documenter, IntervalTrees

makedocs(
    modules = [IntervalTrees],
    format = :html,
    sitename = "IntervalTrees",
    pages = [
        "Home" => "index.md",
        "Manual" => [
            "Types" => "man/types.md",
            "Creating IntervalTrees" => "man/insertion.md",
            "Dictionary Operations" => "man/dict.md",
            "Iterating over intervals" => "man/iteration.md",
            "Intersection" => "man/intersection.md"
        ]
    ],
    authors = "Daniel Jones, Kenta Sato, Ben J. Ward, The BioJulia Organisation and other contributors."
)

deploydocs(
    repo = "github.com/BioJulia/IntervalTrees.jl.git",
    julia = "0.7",
    osname = "linux",
    target = "build",
    deps = nothing,
    make = nothing
)
