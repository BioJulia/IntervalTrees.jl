var documenterSearchIndex = {"docs": [

{
    "location": "index.html#",
    "page": "Home",
    "title": "Home",
    "category": "page",
    "text": ""
},

{
    "location": "index.html#IntervalTrees-1",
    "page": "Home",
    "title": "IntervalTrees",
    "category": "section",
    "text": "(Image: Latest Release) (Image: MIT License) (Image: Stable documentation) (Image: Lifecycle) (Image: Chat on Discord)"
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
    "text": "CurrentModule = IntervalTrees"
},

{
    "location": "man/types.html#IntervalTrees.AbstractInterval",
    "page": "Types",
    "title": "IntervalTrees.AbstractInterval",
    "category": "type",
    "text": "Types deriving from AbstractInterval{T} must have a first and last function each returning a value of type T, and first(i) <= last(i) must always be true.\n\n\n\n\n\n"
},

{
    "location": "man/types.html#IntervalTrees.Interval",
    "page": "Types",
    "title": "IntervalTrees.Interval",
    "category": "type",
    "text": "A basic interval.\n\n\n\n\n\n"
},

{
    "location": "man/types.html#IntervalTrees.IntervalValue",
    "page": "Types",
    "title": "IntervalTrees.IntervalValue",
    "category": "type",
    "text": "An interval with some associated data.\n\n\n\n\n\n"
},

{
    "location": "man/types.html#Intervals-and-IntervalTrees-1",
    "page": "Types",
    "title": "Intervals & IntervalTrees",
    "category": "section",
    "text": "IntervalTrees exports an abstract type AbstractInterval{K}, in addition to two basic concrete interval types.AbstractInterval\nInterval\nIntervalValueIntervals in this package are always treated as end-inclusive, similar to the Julia Range type."
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

{
    "location": "CONTRIBUTING.html#",
    "page": "Contributing",
    "title": "Contributing",
    "category": "page",
    "text": ""
},

{
    "location": "CONTRIBUTING.html#Contributing-to-BioJulia-1",
    "page": "Contributing",
    "title": "Contributing to BioJulia",
    "category": "section",
    "text": ":+1::tada: First off, thanks for taking the time to contribute! :tada::+1:The following is a set of guidelines for contributing to BioJulia repositories, which are hosted in the BioJulia Organization on GitHub.These are mostly guidelines, not rules. Use your best judgment, and feel free to propose changes to this document in a pull request."
},

{
    "location": "CONTRIBUTING.html#Table-of-contents-1",
    "page": "Contributing",
    "title": "Table of contents",
    "category": "section",
    "text": "I don\'t want to read this whole thing, I just have a question!!!What should I know about BioJulia before I get started?BioJulia Package Maintainers\nBioJulia Administrators\nEtiquette and conduct\nPackage ConventionsHow Can I Contribute?Reporting Bugs\nSuggesting an Enhancement\nMaking Pull Requests\nBecome a BioJulia package maintainerStyleguidesGit Commit Messages\nAdditional julia style suggestions\nDocumentation StyleguideAdditional notesA suggested branching model"
},

{
    "location": "CONTRIBUTING.html#I-don\'t-want-to-read-this-whole-thing-I-just-have-a-question!!!-1",
    "page": "Contributing",
    "title": "I don\'t want to read this whole thing I just have a question!!!",
    "category": "section",
    "text": "We understand you are excited to get involved already! But please don\'t file an issue to ask a question. You\'ll get faster results by using the resources below.We have a Gitter message chat room where the community chimes in with helpful advice if you have questions. If you just have a question, or a problem that is not covered by this guide, then come on over to the Gitter and we\'ll be happy to help.Gitter, BioJulia message board"
},

{
    "location": "CONTRIBUTING.html#What-should-I-know-about-BioJulia-**BEFORE**-I-get-started?-1",
    "page": "Contributing",
    "title": "What should I know about BioJulia BEFORE I get started?",
    "category": "section",
    "text": ""
},

{
    "location": "CONTRIBUTING.html#BioJulia-Package-Maintainers-1",
    "page": "Contributing",
    "title": "BioJulia Package Maintainers",
    "category": "section",
    "text": "In order to provide the best possible experience for new and existing users of Julia from the life-sciences, a little bit of structure and organization is necessary.Each package is dedicated to introducing a specific data type or algorithm, or dealing with a specific biological problem or pipeline.Whilst there are some \"meta-packages\" such as Bio.jl, which bundle individual packages together for convenience of installation and use, most of the BioJulia software ecosystem is quite decentralized.Therefore, it made sense that maintenance of the packages should also be fairly decentralized, to achieve this, we created the role of a \"Package Maintainer\".The maintainer(s) for a given package are listed in the packages README.md file.The maintainers of a package are responsible for the following aspects of the package they maintain.Deciding the branching model used and how branches are protected.\nReviewing pull requests, and issues for that package.\nTo tag releases of a package at suitable points in the lifetime of the package.\nTo be considerate and of assistance to new contributors, new community members and new maintainers.\nTo report potential incidents of antisocial to a BioJulia admin member.See HERE for extra guidance and suggestions on branching models and tagging releases.Package maintainers hold admin level access for any package(s) for which they are listed as maintainer, and so new contributors to BioJulia should rest assured they will not be \'giving up\' any package they transfer to BioJulia, they shall remain that package\'s administrator. Package maintainers also have push (but not admin) access to all other code packages in the BioJulia ecosystem.This allows for a community spirit where maintainers who are dedicated primarily to other packages may step in to help other maintainers to resolve a PR or issue. As such, newer maintainers and researchers contributing a package to the BioJulia ecosystem can rest assured help will always be at hand from our community.However, if you are a maintainer stepping in to help the maintainer(s) dedicated to another package, please respect them by first offering to step in and help, before changing anything. Secondly, ask them before doing advanced and potentially destructive git operations e.g forcing pushes to branches (especially master), or re-writing history of branches. Please defer to the judgement of the maintainers dedicated in the README of the package."
},

{
    "location": "CONTRIBUTING.html#BioJulia-Administrators-1",
    "page": "Contributing",
    "title": "BioJulia Administrators",
    "category": "section",
    "text": "BioJulia has a select group of members in an Admin team. This team has administrative access to all repositories in the BioJulia project.The admin team is expected to:Respond and resolve any disputes between any two BioJulia contributors.\nAct as mentors to all other BioJulia maintainers.\nAssist maintainers in the upkeep of packages when requested. Especially when more difficult re-bases and history manipulation are required.\nSome administrators maintain the BioJulia infrastructure. This includes being responsible for the accounts and billing of any platforms used by BioJulia, and the maintenance of any hardware like servers owned and used by BioJulia."
},

{
    "location": "CONTRIBUTING.html#Etiquette-and-conduct-1",
    "page": "Contributing",
    "title": "Etiquette and conduct",
    "category": "section",
    "text": "BioJulia outlines a statement of etiquette and conduct that all members and contributors are expected to uphold. Please take the time to read and understand this statement."
},

{
    "location": "CONTRIBUTING.html#Package-conventions-1",
    "page": "Contributing",
    "title": "Package conventions",
    "category": "section",
    "text": "First, be familiar with the official julia documentation on:Packages\nPackage Development\nModulesPackage names should be a simple and self explanatory as possible, avoiding unneeded acronyms.Packages introducing some key type or method/algorithm should be named accordingly.For example, the BioJulia package introducing biological sequence types and functionality to process sequence data is called \"BioSequences\". GitHub repository names of BioJulia packages should end in .jl, even though the package name itself does not. i.e. \"BioSequences\" is the name of the package, and the name of its GitHub repository is \"BioSequences.jl\".Considerate and simple naming greatly assists people in finding the kind of package or functionality they are looking for.File names of files containing julia code in packages should end in .jl.All user facing types and functions (i.e. all types and functions exported from the module of a package), should be documented. Documentation regarding specific implementation details that aren\'t relevant to users should be in the form of comments. Please DO comment liberally for complex pieces of code!We use Documenter.jl, to generate user and developer documentation and host it on the web. The source markdown files for such manuals is kept in the docs/src/ folder of each BioJulia package/repository.The code in all BioJulia packages is unit tested. Such tests should be organized into contexts, and into separate files based on module.Files for tests for a module go into an appropriately named folder, within the test folder in the git repo."
},

{
    "location": "CONTRIBUTING.html#How-can-I-contribute?-1",
    "page": "Contributing",
    "title": "How can I contribute?",
    "category": "section",
    "text": ""
},

{
    "location": "CONTRIBUTING.html#Reporting-Bugs-1",
    "page": "Contributing",
    "title": "Reporting Bugs",
    "category": "section",
    "text": "Here we show you how to submit a bug report for a BioJulia repository. If you follow the advice here, BioJulia maintainers and the community will better understand your report :pencil:, be able to reproduce the behaviour :computer: :computer:, and identify related problems :mag_right:."
},

{
    "location": "CONTRIBUTING.html#Before-creating-a-bug-report:-1",
    "page": "Contributing",
    "title": "Before creating a bug report:",
    "category": "section",
    "text": "Please do the following:Check the GitHub issue list for the package that is giving you problems.\nIf you find an issue already open for your problem, add a comment to leteveryone know that you are experiencing the same issue.If no currently open issue already exists for your problem that has already been then you should create a new issue.\nNote: If you find a Closed issue that seems like it is the same thing that you\'re experiencing, open a new issue and include a link to the original issue in the body of your new one."
},

{
    "location": "CONTRIBUTING.html#How-to-create-a-(good)-new-bug-report:-1",
    "page": "Contributing",
    "title": "How to create a (good) new bug report:",
    "category": "section",
    "text": "Bugs are tracked as GitHub issues. After you\'ve determined which repository your bug is related to, create an issue on that repository and provide the following information by filling in template. This template will help you to follow the guidance below.When you are creating a bug report, please do the following:Explain the problem\nUse a clear and descriptive title for the issue to identify the problem.\nDescribe the exact steps which reproduce the problem in as many details as possible.\nWhich function / method exactly you used?\nWhat arguments or parameters were used?\nProvide a specific example. (Includes links to pastebin, gists and so on.) If you\'re providing snippets in the issue, use Markdown code blocks.\nDescribe the behaviour you observed after following the steps\nPoint out what exactly is the problem with that behaviour.\nExplain which behaviour you expected to see instead and why.\nOPTIONALLY: Include screenshots and animated GIFs which show you following the described steps and clearly demonstrate the problem. You can use this tool to record GIFs on macOS and Windows, or this tool or this tool on Linux.\nProvide additional context for the problem (some of these may not always apply)\nDid the problem start happening recently (e.g. after updating to a new version)?\nIf the problem started recently, can you reproduce the problem in older versions?\nDo you know the most recent package version in which the problem doesn\'t happen?\nCan you reliably reproduce the issue? If not...\nProvide details about how often the problem happens.\nProvide details about under which conditions it normally happens.\nIs the problem is related to working with files? If so....\nDoes the problem happen for all files and projects or only some?\nDoes the problem happen only when working with local or remote files?\nDoes the problem happen for files of a specific type, size, or encoding?\nIs there anything else special about the files you are using?\nInclude details about your configuration and environmentWhich version of the package are you using?\nWhat\'s the name and version of the OS you\'re using?\nWhich julia packages do you have installed?\nAre you using local configuration files to customize julia behaviour? If so...\nPlease provide the contents of those files, preferably in a\ncode block or with a link to a gist.Note: All of the above guidance is included in the template for your convenience."
},

{
    "location": "CONTRIBUTING.html#Suggest-an-Enhancement-1",
    "page": "Contributing",
    "title": "Suggest an Enhancement",
    "category": "section",
    "text": "This section explains how to submit an enhancement proposal for a BioJulia package. This includes completely new features, as well as minor improvements to existing functionality. Following these suggestions will help maintainers and the community understand your suggestion :pencil: and find related suggestions :mag_right:."
},

{
    "location": "CONTRIBUTING.html#Before-Submitting-An-Enhancement-Proposal-1",
    "page": "Contributing",
    "title": "Before Submitting An Enhancement Proposal",
    "category": "section",
    "text": "Check if there\'s already a package which provides that enhancement.\nDetermine which package the enhancement should be suggested in.\nPerform a cursory issue search to see if the enhancement has already been suggested.\nIf it has not, open a new issue as per the guidance below.\nIf it has...\nAdd a comment to the existing issue instead of opening a new one.\nIf it was closed, take the time to understand why this was so (it\'s ok to ask! :) ), and consider whether anything has changed that makes the reason outdated. If you can think of a convincing reason to reconsider the enhancement, feel free to open a new issue as per the guidance below."
},

{
    "location": "CONTRIBUTING.html#How-to-submit-a-(good)-new-enhancement-proposal-1",
    "page": "Contributing",
    "title": "How to submit a (good) new enhancement proposal",
    "category": "section",
    "text": "Enhancement proposals are tracked as GitHub issues. After you\'ve determined which package your enhancement proposals is related to, create an issue on that repository and provide the following information by filling in template. This template will help you to follow the guidance below.Explain the enhancement\nUse a clear and descriptive title for the issue to identify the suggestion.\nProvide a step-by-step description of the suggested enhancement in as many details as possible.\nProvide specific examples to demonstrate the steps. Include copy/pasteable snippets which you use in those examples, as Markdown code blocks.\nIf you want to change current behaviour...\nDescribe the current behaviour.\nExplain which behaviour you expected to see instead and why.\nWill the proposed change alter APIs or existing exposed methods/types? If so, this may cause dependency issues and breakages, so the maintainer will need to consider this when versioning the next release.\nOPTIONALLY: Include screenshots and animated GIFs. You can use this tool to record GIFs on macOS and Windows, and this tool or this tool on Linux.\nProvide additional context for the enhancement\nExplain why this enhancement would be useful to most BioJulia users and isn\'t something that can or should be implemented as a separate package.\nDo you know of other projects where this enhancement exists?\nInclude details about your configuration and environment\nSpecify which version of the package you\'re using.\nSpecify the name and version of the OS you\'re using.Note: All of the above guidance is included in the template for your convenience."
},

{
    "location": "CONTRIBUTING.html#Making-Pull-Requests-1",
    "page": "Contributing",
    "title": "Making Pull Requests",
    "category": "section",
    "text": "BioJulia packages (and all julia packages) can be developed locally. For information on how to do this, see this section of the julia documentation.Before you start working on code, it is often a good idea to open an enhancement suggestionOnce you decide to start working on code, the first thing you should do is make yourself an account on Github. The chances are you already have one if you\'ve done coding before and wanted to make any scripts or software from a science project public.The first step to contributing is to find the BioJulia repository for the package. Hit the \'Fork\' button on the repositories page to create a forked copy of the package for your own Github account. This is your blank slate to work on, and will ensure your work and experiments won\'t hinder other users of the released and stable package.From there you can clone your fork of the package and work on it on your machine using git. Here\'s an example of cloning, assuming you already forked the BioJulia package \"BioSequences.jl\":git clone https://github.com/<YOUR_GITHUB_USERNAME_HERE>/BioSequences.jl.gitGit will download or \"clone\" your fork and put it in a folder called BioSequences.jl it creates in your current directory.It is beyond the scope of this document to describe good git and github use in more specific detail, as the folks at Git and GitHub have already done that wonderfully on their own sites. If you have additional questions, simply ping a BioJulia member or the BioJulia Gitter."
},

{
    "location": "CONTRIBUTING.html#How-to-make-(good)-code-contributions-and-new-Pull-Requests-1",
    "page": "Contributing",
    "title": "How to make (good) code contributions and new Pull-Requests",
    "category": "section",
    "text": "In your code changes\nBranch properly!\nIf you are making a bug-fix, then you need to checkout your bug-fix branch from the last release tag.\nIf you are making a feature addition or other enhancement, checkout your branch from master.\nSee here for more information (or ask a package maintainer :smile:).\nFollow the julia style guide.\nFollow the additional style suggestions.\nFollow the julia performance tips.\nUpdate and add docstrings for new code, consistent with the documentation styleguide.\nUpdate information in the documentation located in the docs/src/ folder of the package/repository if necessary.\nEnsure that unit tests have been added which cover your code changes.\nEnsure that you have added an entry to the [UNRELEASED] section of the manually curated CHANGELOG.md file for the package. Use previous entries as an example. Ensure the CHANGELOG.md is consistent with the\nrecommended changelog style.\nAll changes should be compatible with the latest stable version of Julia.\nPlease comment liberally for complex pieces of internal code to facilitate comprehension.\nIn your pull request\nUse the pull request template\nDescribe the changes in the pull request\nProvide a clear, simple, descriptive title.\nDo not include issue numbers in the PR title.\nIf you have implemented new features or behaviour\nProvide a description of the addition in as many details as possible.\nProvide justification of the addition.\nProvide a runnable example of use of your addition. This lets reviewers and others try out the feature before it is merged or makes it\'s way to release.\nIf you have changed current behaviour...\nDescribe the behaviour prior to you changes\nDescribe the behaviour after your changes and justify why you have made the changes.\nDoes your change alter APIs or existing exposed methods/types? If so, this may cause dependency issues and breakages, so the maintainer will need to consider this when versioning the next release.\nIf you are implementing changes that are intended to increase performance, you should provide the results of a simple performance benchmark exercise demonstrating the improvement. Especially if the changes make code less legible.Note: All of the above guidance is included in the template for your convenience."
},

{
    "location": "CONTRIBUTING.html#Reviews-and-merging-1",
    "page": "Contributing",
    "title": "Reviews and merging",
    "category": "section",
    "text": "You can open a pull request early on and push changes to it until it is ready, or you can do all your editing locally and make a pull request only when it is finished - it is up to you.When your pull request is ready on Github, mention one of the maintainers of the repo in a comment e.g. @Ward9250 and ask them to review it. You can also use Github\'s review feature. They will review the code and documentation in the pull request, and will assess it.Your pull request will be accepted and merged if:The dedicated package maintainers approve the pull request for merging.\nThe automated build system confirms that all unit tests pass without any issues.There may be package-specific requirements or guidelines for contributors with some of BioJulia\'s packages. Most of the time there will not be, the maintainers will let you know.It may also be that the reviewers or package maintainers will want to you to make changes to your pull request before they will merge it. Take the time to understand why any such request has been made, and freely discuss it with the reviewers. Feedback you receive should be constructive and considerate (also see here)."
},

{
    "location": "CONTRIBUTING.html#Submitting-a-package-to-BioJulia-1",
    "page": "Contributing",
    "title": "Submitting a package to BioJulia",
    "category": "section",
    "text": "If you have written a package, and would like to have it listed under - and endorsed by - the BioJulia organization, you\'re agreeing to the following:Allowing BioJulia to have joint ownership of the package. This is so that the members can help you review and merge pull requests and other contributions, and also help you to develop new features. This policy ensures that you (as the package author and current maintainer) will have good support in maintaining your package to the highest possible quality.\nGo through a joint review/decision on a suitable package name. This usually the original package name. However, package authors may be asked to rename their package to something more official and discoverable (by search engines and such) if it is contentious or non-standard.To submit your package, follow these steps:Introduce yourself and your package on the BioJulia Gitter channel.\nAt this point maintainers will reach out to mentor and vouch for you and your package. They will:\nDiscuss with you a suitable name.\nHelp you ensure the the package is up to standard, and meets the code and contribution guidelines described on this site.\nAdd you to the BioJulia organisation if you wish to become a BioJulia maintainer.\nTransfer ownership of the package."
},

{
    "location": "CONTRIBUTING.html#Become-a-BioJulia-package-maintainer-1",
    "page": "Contributing",
    "title": "Become a BioJulia package maintainer",
    "category": "section",
    "text": "You may ask the current admin or maintainers of a BioJulia package to invite you.They will generally be willing to do so if you have done one or more of the following to contribute to BioJulia in the past:You have submitted a new package to BioJulia.\nReported a bug.\nSuggested enhancements.\nMade one or more pull requests implementing one or more...\nFixed bugs.\nImproved performance.\nAdded new functionality.\nIncreased test coverage.\nImproved documentation.None of these requirements are set in stone, but we prefer you to have done one or more of the above, as it gives good confidence that you are familiar with the tasks and responsibilities of maintaining a package used by others, and are willing to do so. Any other avenue for demonstrating commitment to the community and the GitHub organisation will also be considered."
},

{
    "location": "CONTRIBUTING.html#BioJulia-members-can-sometimes-become-administrators-1",
    "page": "Contributing",
    "title": "BioJulia members can sometimes become administrators",
    "category": "section",
    "text": "Members of the admin team have often been contributing to BioJulia for a long time, and may even be founders present at the inception of the project. In order to become an admin, one does not necessarily have to contribute large amounts of code to the project. Rather the decision to on-board a member to an admin position requires a history of using and contributing to BioJulia, and a positive interaction and involvement with the community. Any BioJulia member fulfilling this, may offer to take on this responsibility."
},

{
    "location": "CONTRIBUTING.html#Styleguides-1",
    "page": "Contributing",
    "title": "Styleguides",
    "category": "section",
    "text": ""
},

{
    "location": "CONTRIBUTING.html#Git-Commit-messages-1",
    "page": "Contributing",
    "title": "Git Commit messages",
    "category": "section",
    "text": "Use the present tense (\"Add feature\" not \"Added feature\").\nUse the imperative mood (\"Move cursor to...\" not \"Moves cursor to...\").\nLimit the first line to 72 characters or less.\nReference issues and pull requests liberally after the first line.\nConsider starting the commit message with an applicable emoji:\n:art: :art: when improving the format/structure of the code\n:racehorse: :racehorse: when improving performance\n:memo: :memo: when writing docs\n:penguin: :penguin: when fixing something on Linux\n:apple: :apple: when fixing something on macOS\n:checkeredflag: `:checkeredflag:` when fixing something on Windows\n:bug: :bug: when fixing a bug\n:fire: :fire: when removing code or files\n:greenheart: `:greenheart:` when fixing the CI build\n:whitecheckmark: :white_check_mark: when adding tests\n:arrowup: `:arrowup:` when upgrading dependencies\n:arrowdown: `:arrowdown:` when downgrading dependencies\n:exclamation: :exclamation: when removing warnings or depreciations"
},

{
    "location": "CONTRIBUTING.html#Additional-julia-style-suggestions-1",
    "page": "Contributing",
    "title": "Additional julia style suggestions",
    "category": "section",
    "text": "Source code files should have the following style of header:\n# Title\n# =====\n#\n# Short description.\n#\n# [Long description (optional)]\n#\n# This file is a part of BioJulia. License is MIT: <link to the license file>\nIndent with 4 spaces.\nFor functions that are not a single expression, it is preferred to use an explicit return. Be aware that functions in julia implicitly return the the result of the last expression in the function, so plain return should be used to indicate that the function returns nothing.\nType names are camel case, with the first letter capitalized. E.g. SomeVeryUsefulType.\nModule names should be camel case.\nSeparate logical blocks of code with one blank line. Although it is common and acceptable for short single-line functions to be defined together on consecutive lines with no blank lines between them.\nFunction names, apart from constructors, are all lowercase. Include underscores between words only if the name would be hard to read without. E.g.  start, stop, find_letter find_last_digit. It is good to separate concepts in a name with a _.\nGenerally try to keep lines below 100-columns, unless splitting a long line onto multiple lines makes it harder to read.\nFiles that declare modules should only declare the module, and import any modules that it requires. Any subsequent significant code should be included from separate files. E.g.module AwesomeFeatures\n\nusing IntervalsTrees, JSON\n\ninclude(\"feature1.jl\")\ninclude(\"feature2.jl\")\n\nendFiles that declare modules should have the same name name of the module. E.g the module SomeModule is declared under the file SomeModule.jl.\nWhen extending method definitions, define the methods with a module name prefix. E.g.function Base.start(iter::YourType)\n  ...\nend\n\nBase.done(iter::YourType, state) = ...Functions that get or set variables in a struct should not be prefixed with \'get\' or \'set\'. The getter should be named for the variable it gets, and the setter should have the same name as the getter, with the suffix !. For example, for the variable names:name(node) # get node name\nname!(node, \"somename\") # set node nameWhen using conditional branching, if code is statement-like, an if-else block should be used. However if the code is expression-like then julia\'s ternary operator should be used.\nmatches == sketchlen ? 1.0 : matches / (2 * sketchlen - matches)\nSome simple checks and expressions are also expressed using the && or || operators instead of if-else syntax. For example:\nisvalid(foo) || throw(ArgumentError(\"$foo is not valid\"))"
},

{
    "location": "CONTRIBUTING.html#Additional-Notes-1",
    "page": "Contributing",
    "title": "Additional Notes",
    "category": "section",
    "text": ""
},

{
    "location": "CONTRIBUTING.html#A-suggested-branching-model-1",
    "page": "Contributing",
    "title": "A suggested branching model",
    "category": "section",
    "text": "If you are a dedicated maintainer on a BioJulia package, you may be wondering which branching model to choose for development and maintenance of your code.If you are a contributor, knowing the branching model of a package may help you work more smoothly with the maintainer of the package.There are several options available, including git-flow.Below is a recommended branching model for your repo, but it is only a suggestion. What is best for you as the dedicated maintainer(s), is best for you.The model below is a brief summary of the \'OneFlow model\'. We describe it in summary here for convenience, but we recommend you check out the blog article as a lot more justification and reasoning is presented on why this model is the way it is."
},

{
    "location": "CONTRIBUTING.html#During-development-1",
    "page": "Contributing",
    "title": "During development",
    "category": "section",
    "text": "There is only one main branch - you can call it anything, but usually it\'s called master.\nUse temporary branches for features, releases, and bug-fixes. These temporary branches are used as a convenience to share code with other developers and as a backup measure. They are always removed once the changes present on them are added to master.\nFeatures are integrated onto the master branch primarily in a way which keeps the history linear and simple. A good compromise to the rebase vs. merge commit debate for this step is to first do an interactive rebase of the feature branch on master, and then do a non-fast-forward merge. Github now does squashed commits when merging a PR and this is fine too.Feature Example:git checkout -b feature/my-feature master\n\n... Make commits to feature/my-feature to finish the feature ...\n\ngit rebase -i master\ngit checkout master\ngit merge --no-ff feature/my-feature\ngit push origin master\ngit branch -d feature/my-feature"
},

{
    "location": "CONTRIBUTING.html#:sparkles:-Making-new-releases-1",
    "page": "Contributing",
    "title": ":sparkles: Making new releases",
    "category": "section",
    "text": "You create a new branch for a new release. It branches off from master at the point that you decided master has all the necessary features. This is not necessarily the tip of the master branch.\nFrom then on new work, aimed for the next release, is pushed to master as always, and any necessary changes for the current release are pushed to the release branch. Once the release is ready, you tag the top of the release branch.\nOnce the release is ready, tag the top of the release branch with a version number. Then do a typical merge of the release branch into master. Any changes that were made during the release will now be part of master. Delete the release branch.Release Example:git checkout -b release/2.3.0 9efc5d\n\n... Make commits to release/2.3.0 to finish the release ...\n\ngit tag 2.3.0\ngit checkout master\ngit merge release/2.3.0\ngit push --tags origin master\ngit branch -d release/2.3.0\ngit push origin :release/2.3.0Do your pushes, and go to GitHub to make your release available."
},

{
    "location": "CONTRIBUTING.html#:bug:-Hot-fixes-and-hot-fix-releases-1",
    "page": "Contributing",
    "title": ":bug: Hot-fixes and hot-fix releases",
    "category": "section",
    "text": "When a hot-fix is needed, create a hot-fix branch, that branches from the release tag that you want to apply the fix to.\nPush the needed fixes to the hot-fix branch.\nWhen the fix is ready, tag the top of the fix branch with a new release,  merge it into master, finally delete the hot-fix branch.Hot-fix example:git checkout -b hotfix/2.3.1 2.3.0\n\n... Add commits which fix the problem ...\n\ngit tag 2.3.1\ngit checkout master\ngit merge hotfix/2.3.1\ngit push --tags origin master\ngit branch -d hotfix/2.3.1IMPORTANT: There is one special case when finishing a hot-fix branch. If a release branch has already been cut in preparation for the next release before the hot-fix was finished, you need to merge the hot-fix branch not to master, but to that release branch."
},

{
    "location": "CODE_OF_CONDUCT.html#",
    "page": "Code of Conduct",
    "title": "Code of Conduct",
    "category": "page",
    "text": ""
},

{
    "location": "CODE_OF_CONDUCT.html#Etiquette-and-conduct-in-BioJulia-1",
    "page": "Code of Conduct",
    "title": "Etiquette and conduct in BioJulia",
    "category": "section",
    "text": "As you interact with other members of the BioJulia group, or make contributions you may have revisions and suggestions on your pull request from BioJulia members or others which they want to be implemented before they will merge your pull request.You may also have disagreements with people on the forums or chats maintained by BioJulia.In order to keep BioJulia a civil and enjoyable place, where technical disagreements and issues can be discussed and resolved in a mature and constructive way, we outline three principles of etiquette we expect members and contributors to abide by.Anybody violating these principles in order to upset any member or contributor may be flagged to the BioJulia admins who will decide on an appropriate course of action. This includes locking conversations for cool-off periods, or even bans of individuals.This statement on etiquette is not an exhaustive list of things that you can or can’t do. Rather, it is a statement of our spirit and attitude towards interacting with each other.This statement applies in all spaces managed by the BioJulia organisation. This includes any gitter, mailing lists, issue trackers, repositories, or any other forums used by BioJulia for communication (such as Skype, Google Hangouts, etc). It also applies in real-world events and spaces organised by BioJulia."
},

{
    "location": "CODE_OF_CONDUCT.html#The-principles-of-etiquette-1",
    "page": "Code of Conduct",
    "title": "The principles of etiquette",
    "category": "section",
    "text": ""
},

{
    "location": "CODE_OF_CONDUCT.html#.-Be-welcoming,-friendly-and-patient.-1",
    "page": "Code of Conduct",
    "title": "1. Be welcoming, friendly and patient.",
    "category": "section",
    "text": "Be welcoming. We strive to welcome and support any individual participating in BioJulia activities to any extent (from developing code, to support seeking users). We have even been known to have a few members on our Gitter who are not Biologists, but they enjoy the forum, like what we do, and stick around for the programming chat. All are welcome (yes including you! :smile:)."
},

{
    "location": "CODE_OF_CONDUCT.html#.-Be-considerate.-1",
    "page": "Code of Conduct",
    "title": "2. Be considerate.",
    "category": "section",
    "text": "Your work will be used by other people, and you in turn will depend on the work of others. From any code you make, to any support questions you ask or answer! Any decision you take will affect users and colleagues, and you should take those consequences into account when making decisions.Remember that we\'re a world-wide community, so you might not be communicating in someone else\'s primary language."
},

{
    "location": "CODE_OF_CONDUCT.html#.-Be-respectful.-1",
    "page": "Code of Conduct",
    "title": "3. Be respectful.",
    "category": "section",
    "text": "Not all of us will agree all the time, but disagreement is no excuse for poor behaviour and poor manners. We might all experience some frustration now and then, but we cannot allow that frustration to turn into a personal attack. It’s important to remember that a community where people feel uncomfortable or threatened is not a productive or fun community. Members of the BioJulia community should be respectful when dealing with other members as well as with people outside the BioJulia community.Please do not insult or put down other participants. Harassment and other exclusionary behaviour is not acceptable. This includes, but is not limited to:Violent threats or language directed against another person.\nPrejudiced, bigoted, or intolerant, jokes and language.\nPosting sexually explicit or violent material.\nPosting (or threatening to post) other people\'s personally identifying information (\"doxing\").\nPersonal insults, especially those using racist or sexist terms.\nUnwelcome sexual attention.\nAdvocating for, or encouraging, any of the above behaviour.\nRepeated harassment of others. In general, if someone asks you to stop, then stop.When we disagree, try to understand why. Disagreements, both social and technical, happen all the time and this community is unlikely to be any exception! It is important that we resolve disagreements and differing views constructively. Different people have different perspectives on issues. Being unable to understand why someone holds a viewpoint doesn’t mean that they’re wrong. Don’t forget that it is human to err and blaming each other doesn’t get us anywhere. Instead, focus on helping to resolve issues and learning from mistakes.Assume the person you have a disagreement with really does want the best for BioJulia, just as you do. Therefore, if you are ever unsure what the meaning or tone of a comment may be, in the first instance, assume your fellow BioJulia member is acting in good faith, this may well be a mistake in communication (with the scientific community as diverse as it is, such mis-steps are likely!). If you are comfortable doing so, ask them to clarify what they mean or to rephrase their point. If you don\'t feel comfortable doing this, or if it is clear the behaviour is hostile and not acceptable, please report it (see next section)."
},

{
    "location": "CODE_OF_CONDUCT.html#Is-someone-behaving-inappropriately?-1",
    "page": "Code of Conduct",
    "title": "Is someone behaving inappropriately?",
    "category": "section",
    "text": "If you are affected by the behaviour of a member or contributor of BioJulia, we ask that you report it by contacting the BioJulia Admin Team collectively, by emailing admin@biojulia.net. They will get back to you and begin to resolve the situation. In some cases we may determine that a public statement will need to be made. If that\'s the case, the identities of all involved will remain confidential unless those individuals instruct us otherwise.Ensure to include in your email:Your contact info (so we can get in touch with you if we need to follow up).\nNames (real, nicknames, or pseudonyms) of any individuals involved. If there were other witnesses besides you, please try to include them as well.\nWhen and where the incident occurred. Please be as specific as possible.\nYour account of what occurred. If there is a publicly available record (e.g. a mailing list archive or a public IRC logger) please include a link.\nAny extra context you believe existed for the incident.\nIf you believe this incident is ongoing.\nAny other information you believe we should have.s"
},

]}
