<a name="top"> </a>

[This document is formatted with GitHub-Flavored Markdown.              ]:#
[For better viewing, including hyperlinks, read it online at            ]:#
[https://github.com/sourceryinstitute/FEATS/blob/master/README.md]:#
<div align="center">

![Sourcery Institute][sourcery-institute logo]


Framework for Extensible Asynchronous Task Scheduling (FEATS)
=============================================================

[![GitHub license][license img]](./LICENSE)
[![GitHub release][release img]](https://github.com/sourceryinstitute/FEATS/releases/latest)
[![Download as PDF][pdf img]](https://md2pdf.herokuapp.com/sourceryinstitute/FEATS/blob/master/README.pdf)

[Overview](#overview) | [Getting Started](#getting-started) | [Documentation](#documentation) | [Dependencies](#dependencies) | [Acknowledgments](#acknowledgments) | [Donate](#donate)

</div>

Overview
--------
FEATS is a project to develop a parallel [Fortran 2018] asynchronous, task-scheduling
framework for use in a range of applications.  Our initial target application will be
[OLTARIS].  The aim is to assign one image per team of images the role of scheduler
with the remaining images serving as compute images.  The scheduler hands out tasks
in an order that respects a directed acyclic graph (DAG) of task dependencies.  

Getting Started
---------------
Please see the [example] directory for a demonstration use case in which FEATS traverses
a DAG that describes the module dependencies within FEATS itself and reports on task
completion at each step.  In this simple example, the task completion is simply printing
what file could have been compiled at the corresponding step.  No actual compiling happens,
but this use case describes what would happen if FEATS were to be used to enable the
 Fortran Package Manager [`fpm`] to perform parallel builds.

With `fpm`, the GNU Fortran compiler ([`gfortran`]), and [OpenCoarrays] installed, build 
FEATS and run the example program by executing the following command in a `bash`-like 
shell:
```
git clone https://github.com/sourceryinstitute/FEATS
cd FEATS
fpm run --example --compiler caf --runner "cafrun -n 4" 
```
Change `4` above to the number of images that you would like to launch in parallel.

Documentation
-------------
Please visit the [FEATS GitHub Pages site] to see HTML documentation generated 
with [`ford`].

Dependencies
-------------
The [`fpm.toml`] manifest describes the FEATS user and developer dependencies
and directs `fpm` to download and build the dependency packages automatically.  

Testing
-------
To build and execute the FEATS test suite, run the following command in a
`bash`-like shell:
```
fpm test --compiler caf --runner "cafrun -n 4"
```

Please report any test failures or other [issues].

Acknowledgments
----------------

| | |
|-|-|
| We gratefully acknowledge support from [NASA Langley Research Center] under contract number 80NSSC20P2246.  | <img src="https://user-images.githubusercontent.com/13108868/112893191-304ce180-908f-11eb-8bea-e0cab5322aa8.png" alt="NASA logo" width="100">|

Donate
------
If you find this software useful, please consider donating code or
[currency](http://bit.ly/donate-to-sourcery-institute) to aid in development efforts.

---

<div align="center">

[![GitHub forks](https://img.shields.io/github/forks/sourceryinstitute/FEATS.svg?style=social&label=Fork)](https://github.com/sourceryinstitute/FEATS/fork)
[![GitHub stars](https://img.shields.io/github/stars/sourceryinstitute/FEATS.svg?style=social&label=Star)](https://github.com/sourceryinstitute/FEATS)
[![GitHub watchers](https://img.shields.io/github/watchers/sourceryinstitute/FEATS.svg?style=social&label=Watch)](https://github.com/sourceryinstitute/FEATS)

</div>

[Hyperlinks]:#

[Overview]: #overview
[Prerequisites]: #prerequisites
[Downloading, building, and testing]: #downloading-building-and-testing
[Acknowledgments]: #acknowledgments
[Donate]: #donate

[`fpm`]: https://github.com/fortran-lang/fpm
[`fpm.toml`]: ./fpm.toml
[`gfortran`]: https://gcc.gnu.org
[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
[OLTARIS]: https://oltaris.nasa.gov
[example]: ./example
[FEATS GitHub Pages site]: https://sourceryinstitute.github.io/FEATS/
[`ford`]: https://github.com/Fortran-FOSS-Programmers/ford
[Fortran 2018]: https://j3-fortran.org/doc/year/18/18-007r1.pdf

[sourcery-institute logo]: http://www.sourceryinstitute.org/uploads/4/9/9/6/49967347/sourcery-logo-rgb-hi-rez-1.png

[NASA Langley Research Center]: https://www.nasa.gov/langley

[issues]: https://github.com/sourceryinstitute/FEATS/issues

[download img]: https://img.shields.io/github/downloads/sourceryinstitute/FEATS/total.svg?style=flat-square "Download count badge"
[license img]: https://img.shields.io/badge/license-BSD--3-blue.svg?style=flat-square "BSD-3 License badge"
[release img]: https://img.shields.io/github/release/sourceryinstitute/FEATS.svg?style=flat-square "Latest release badge"
[pdf img]: https://img.shields.io/badge/PDF-README.md-6C2DC7.svg?style=flat-square "Download this readme as a PDF"
