# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 2018-08-10
### Added
- Substantial changes supporting julia v0.7.0 and v1.0.0.
- Project files, including CHANGELOG, CODE_OF_CONDUCT, and CONTRIBUTING.

### Removed
- :fire: Support for julia v0.6.x.

## [0.4.1] - 2017-11-13
### Changed
- Fixed bugs in `firstintersection` that can lead to missed intersections.

## [0.4.0] - 2017-11-08
### Added
- Support for julia v0.6.

### Changed
- A more precise `issorted` check.

### Removed
- :fire: julia v0.5 support.

## [0.3.0] - 2017-06-23
### Added
- Optional filter predicates to `intersect` and `findfirst` functions.

## [0.2.1] - 2017-06-10
### Changed
- Faster `nextintersection!` function.
- Internal changes in behaviour.
- Fixed constructor syntax.

## [0.2.0] - 2017-03-21
### Added
- Support for julia v0.6.

### Removed
- :fire: Support for julia v0.4.

## [0.1.1] - 2017-02-22

[Unreleased]: https://github.com/BioJulia/IntervalTrees.jl/compare/v1.0.0...HEAD
[1.0.0]: https://github.com/BioJulia/IntervalTrees.jl/compare/v0.4.1...v1.0.0
[0.4.1]: https://github.com/BioJulia/IntervalTrees.jl/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/BioJulia/IntervalTrees.jl/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/BioJulia/IntervalTrees.jl/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/BioJulia/IntervalTrees.jl/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/BioJulia/IntervalTrees.jl/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/BioJulia/IntervalTrees.jl/tree/v0.1.1
