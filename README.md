# `network`:  Classes for Relational Data

[![Build Status](https://travis-ci.org/statnet/network.svg?branch=master)](https://travis-ci.org/statnet/network)
[![Build Status](https://ci.appveyor.com/api/projects/status/3l19hrwv7aamo7ed?svg=true)](https://ci.appveyor.com/project/statnet/network)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/network?color=2ED968)](http://cranlogs.r-pkg.org/)
[![cran version](http://www.r-pkg.org/badges/version/network)](https://cran.r-project.org/package=network)
[![Coverage status](https://codecov.io/gh/statnet/network/branch/master/graph/badge.svg)](https://codecov.io/github/statnet/network?branch=master)
[![R build status](https://github.com/statnet/network/workflows/R-CMD-check/badge.svg)](https://github.com/statnet/network/actions)

Tools to create and modify network objects.  The network class can represent a range of relational data types, and supports arbitrary vertex/edge/graph attributes.

## Latest Windows and MacOS binaries

A set of binaries is built after every commit to the public repository. We strongly encourage testing against them before filing a bug report, as they may contain fixes that have not yet been sent to CRAN. They can be downloaded through the following links:

* [MacOS binary (a `.tgz` file in a `.zip` file)](https://nightly.link/statnet/network/workflows/R-CMD-check.yaml/master/macOS-rrelease-binaries.zip)
* [Windows binary (a `.zip` file in a `.zip` file)](https://nightly.link/statnet/network/workflows/R-CMD-check.yaml/master/Windows-rrelease-binaries.zip)

You will need to extract the MacOS `.tgz` or the Windows `.zip` file from the outer `.zip` file before installing. These binaries are usually built under the latest version of R and their operating system and may not work under other versions.

You may also want to install the corresponding latest binaries for packages on which `network` depends, in particular [`statnet.common`](https://github.com/statnet/statnet.common).
