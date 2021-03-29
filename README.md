<a name="top"> </a>

[This document is formatted with GitHub-Flavored Markdown.              ]:#
[For better viewing, including hyperlinks, read it online at            ]:#
[https://github.com/sourceryinstitute/FEATS/blob/master/README.md]:#
<div align="center">

![Sourcery Institute][sourcery-institute logo]


Framework for Extensible Asynchronous Task Scheduling (FEATS)
=============================================================

[![Release Downloads][download img]][Releases]
[![GitHub license][license img]](./LICENSE)
[![GitHub release][release img]](https://github.com/sourceryinstitute/FEATS/releases/latest)
[![Download as PDF][pdf img]](https://md2pdf.herokuapp.com/sourceryinstitute/FEATS/blob/master/README.pdf)

[Overview](#overview) | [Downloads](#downloads) | [Prerequisites](#prerequisites) | [Installation](#installation) |
[Contributing](#contributing) | [Acknowledgments](#acknowledgments) | [Donate](#donate)

</div>

Overview
--------

FEATS is a project to develop a parallel [Fortran 2018] asynchronous, task-scheduling
framework for use in a range of applications.  Our initial target application will be
[OLTARIS].

Prerequisites
-------------

FEATS is being developed with the packages listed below.

* Fortran Package Manager: [fpm] 0.1.3,
* GNU Compiler Collection [GCC] Fortran compiler: gfortran 10.2, and
* [OpenCoarrays] 2.9.2.

Earlier versions might work as well.

Downloading, building, and testing
----------------------------------
To build and test a parallel run with 2 images, execute the following
commands in a shell on Linux, macOS, or Windows Subsystem for Linux:
```
git clone https://github.com/sourceryinstitute/FEATS
cd FEATS
fpm test --compiler caf --runner "cafrun -n 2"
```
Please report any test failures or other [issues].

Acknowledgments
----------------

| | |
|-|-|
| We gratefully acknowledge support from [NASA Langley Research Center] under contract number 80NSSC20P2246.  | <img src="https://user-images.githubusercontent.com/13108868/112893191-304ce180-908f-11eb-8bea-e0cab5322aa8.png" alt="NASA logo" width="100">
|

Donate
------
If you find this software useful, please consider donating
[your time](CONTRIBUTING.md) or
[your money](http://bit.ly/donate-to-sourcery-institute)
to aid in development efforts.

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

[fpm]: https://github.com/fortran-lang/fpm
[GCC]: https://gcc.gnu.org
[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
[OLTARIS]: https://oltaris.nasa.gov

[Fortran 2018]: https://j3-fortran.org/doc/year/18/18-007r1.pdf

[sourcery-institute logo]: http://www.sourceryinstitute.org/uploads/4/9/9/6/49967347/sourcery-logo-rgb-hi-rez-1.png

[NASA Langley Research Center]: https://www.nasa.gov/langley

[issues]: https://github.com/sourceryinstitute/FEATS/issues

[download img]: https://img.shields.io/github/downloads/sourceryinstitute/FEATS/total.svg?style=flat-square "Download count badge"
[license img]: https://img.shields.io/badge/license-BSD--3-blue.svg?style=flat-square "BSD-3 License badge"
[release img]: https://img.shields.io/github/release/sourceryinstitute/FEATS.svg?style=flat-square "Latest release badge"
[pdf img]: https://img.shields.io/badge/PDF-README.md-6C2DC7.svg?style=flat-square "Download this readme as a PDF"
