# trackR - A simple video tracking software for R

[![Travis-CI Build Status](https://travis-ci.org/swarm-lab/trackR.svg?branch=master)](https://travis-ci.org/swarm-lab/trackR)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/swarm-lab/trackR?branch=master&svg=true)](https://ci.appveyor.com/project/swarm-lab/trackR)

## Description

[`trackR`](https://github.com/swarm-lab/trackR) is an object tracker for R based 
on [OpenCV](https://opencv.org/). It provides an easy-to-use (or so I think) 
graphical interface allowing users to perform basic multi-object video tracking 
in a range of conditions while maintaining individual identities.

`trackR` implements two different methods to detect objects in a video:
1. Background subtraction, which is used when a background image is provided. 
Background subtraction is better suited for situations where the environment in 
which the objects move is stable. 
2. Adaptive thresholding, which is used when no background image is provided. 
Adaptive thresholding is better suited for situations where the environment in 
which the objects move is changing throughout the video (e.g. if the lighting 
conditions are not stable).

`trackR` also allows users to exclude parts of the image by using black and 
white masks that can be easily created using any available image editor. 

Finally, `trackR` borrows several ideas from [`tracktor`](https://github.com/vivekhsridhar/tracktor), 
a command-line video tracking software for Python developed by 
[Vivek Sridhar](https://vhsridhar.wordpress.com/), [Simon Gingins](http://www.simongingins.com/), 
and [Dominique Roche](http://dominiqueroche.weebly.com/). 

---

## Quick start guides

+ [1 - Installation instructions](https://swarm-lab.github.io/trackR/articles/z1_install.html)

---

## FAQ

TODO
