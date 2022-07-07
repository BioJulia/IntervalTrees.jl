using Pkg,
Documenter, IntervalTrees

makedocs(
    format = Documenter.HTML(
        prettyurls = !("local" in ARGS),
        canonical = "https://biojulia.net/BufferedStreams.jl/stable/",
        edit_link = "master",
        sidebar_sitename = false
    ),
    modules = [IntervalTrees],
    sitename = "IntervalTrees.jl",
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
    authors = replace(join(Pkg.TOML.parsefile("Project.toml")["authors"], ", "), r" <.*?>" => "" ) * ", The BioJulia Organisation, and other contributors."
)

deploydocs(
    repo = "github.com/BioJulia/IntervalTrees.jl.git",
    devbranch = "master",
    push_preview = true
)
