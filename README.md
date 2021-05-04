# ScalismoAD

This repository is based on Scalaad(Automatic differentiation for Scala) project that can be
found [here](https://github.com/kogecoo/scalaad).

In this detached-fork I'll attempt to combine [scalaad's](https://github.com/kogecoo/scalaad)
automatic differentiation capabilities
with [Scalismo - Scalable Image Analysis and Shape Modelling](https://github.com/unibas-gravis/scalismo)
framework to improve [MCMC](https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo) sampling process
by leveraging gradient based computations
and [Metropolis-adjusted Langevin algorithm](https://en.wikipedia.org/wiki/Metropolis-adjusted_Langevin_algorithm)
.

All the right goes to the original [author](https://github.com/kogecoo)
until [commit#652ea8e](https://github.com/grigala/ScalismoAD/commit/652ea8e95507a6bdfa7c5cc7146d7f0288607608)
.

## Useful resources

### AD Explanatory Videos

- [Optimization Methods for Machine Learning and Engineering(Chapter 6)](https://youtu.be/YQ7RIHMWA88?list=PLdkTDauaUnQpzuOCZyUUZc0lxf4-PXNR5)
- [Automatic Differentiation Explained with Example](https://youtu.be/jS-0aAamC64)
- [Differentiable Functional Programming by Noel Welsh
  ](https://youtu.be/nETDYWAHAfE)

### Papers

- [The Stan Math Library: Reverse-Mode Automatic Differentiation in C++](https://arxiv.org/abs/1509.07164)
- [A Review of Automatic Differentiation and its Efficient Implementation](https://arxiv.org/abs/1811.05031)

### Scalismo Tutorials

- [1D Bayesian Linear Regression](https://scalismo.org/docs/tutorials/tutorial14)
- [MALA Proposal Draft](https://github.com/unibas-gravis/scalismo/pull/361)
