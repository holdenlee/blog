---
title: On the ability of neural nets to express distributions (at COLT 2017)
subtitle: Holden Lee, Rong Ge, Tengyu Ma, Andrej Risteski, Sanjeev Arora
published: 2017-07-07
modified: 2017-07-07
tags: machine learning, paper, neural network
showTOC: True
inline: True
---

Page to be updated; check back soon.

Shortlink: http://tiny.cc/hlcolt17

# Abstract

Deep neural nets have caused a revolution in many classification tasks. A related ongoing revolution--also theoretically not understood--concerns their ability to serve as generative models for complicated types of data such as images and texts. These models are trained using ideas like variational autoencoders and Generative Adversarial Networks. 

We take a first cut at explaining the expressivity of multilayer nets by giving a sufficient criterion for a function to be approximable by a neural network with $n$ hidden layers. A key ingredient is Barron's Theorem [Barron1993], which gives a Fourier criterion for approximability of a function by a neural network with 1 hidden layer. We show that a composition of n functions which satisfy certain Fourier conditions ("Barron functions") can be approximated by a $n+1$-layer neural network. 

For probability distributions, this translates into a criterion for a probability distribution to be approximable in Wasserstein distance--a natural metric on probability distributions--by a neural network applied to a fixed base distribution (e.g., multivariate gaussian). 
Building up recent lower bound work, we also give an example function that shows that composition of Barron functions is more expressive than Barron functions alone.

# Paper

* [Arxiv page](https://arxiv.org/abs/1702.07028)
* [PDF](https://arxiv.org/pdf/1702.07028).

# Poster

* [Powerpoint](https://www.dropbox.com/s/rjkc0u6estet9sz/barron_poster.pptx?dl=0)
* [PDF](https://www.dropbox.com/s/4niwh4hkro6n882/barron_poster.pdf?dl=0)

# Slides

* Slides to be uploaded.

Thoughts, questions, typos? Leave a comment below.
