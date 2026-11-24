# cosi - A programming language for COde with Scientific Intent

<table style="border-width:0;"><tr><td valign="top" width="250">
  <img src="cosi-logo.png" alt="cosi logo" width="250"/>
  <span style="font-size:7pt">Logo by Penelope Orchard, edited with GPT-6 Luna, <a href="https://creativecommons.org/licenses/by/4.0/">CC-BY 4.0</a></span>
</td><td valign="top">

**cosi** is a (research prototype) programming language for scientific and numerical computing that enables more lightweight, 
description of the intended domain compared to traditional general-purpose programming languages.
 
**System principles/values**. Code should enable:

- _Thinking_;
- _Articulation of human scientific knowledge_;
- _Expressing meaning, intent, and limitations of scientific models_;
- _Computation of predictions in a way that is repeatable, reusable, and reproducible and connected to more abstract descriptions_.

How does this work out technically?

- **Statically-typed**: types are a way to express meaning, but also enforce constraints;
- **Graded numerical types**: types are indexed and carry _structure_ which is used to express domain-specific properties in the type system;
- **Python-like syntax**: To lower barrier to entry, cosi's syntax is based on [PurePy](https://pure-py.github.io/), a pure (side-effect free) subset of Python.  

</td></tr></table>

## Example and usage

From [examples/quantity.cosi](examples/quantity.cosi):

```cosi
"""
Simple example showing units and quantities
"""
x : Float[Unit[m] & Quantity[length] & Basis[Obj1]] = 2.0
y : Float[Unit[m] & Quantity[length] & Basis[Obj1]] = 1.0
t : Float[Unit[s] & Quantity[time] & Basis[Obj1]] = 4.0
# End result
it : Float[Unit[m / s] & Quantity[length / time] & Basis[Obj1]] = (x + y) / t
```
Running `cosi examples/quantity.cosi` (or just `cosi examples/quantity`; the `.cosi` extension is optional) produces:
```
Well-typed as Float[{(UoM & KoQ & BasisTy)}][(Unit[(m * s^-1.0)] & Quantity[(length * time^-1.0) & & Basis[Obj1]])]
0.75
```
The type checker verifies the units and quantities are consistent, and evaluates the result `(2.0 + 1.0) / 4.0 = 0.75` with inferred type equivalent
to `Float[Unit[m/s] & Quantity[length/time] & Basis[Obj1]]`.

### More examples

Some examples that show off the main ideas (the files in [examples/demo](examples/demo) build up these ideas step by step):

**Units and dimensions**
- [examples/demo/1-dimensions.cosi](examples/demo/1-dimensions.cosi): hydrostatic balance, written purely in terms of dimensions
- [examples/flow-rate.cosi](examples/flow-rate.cosi): Torricelli's law for flow through an orifice, with units checked through `sqrt`
- [examples/energy-balance.cosi](examples/energy-balance.cosi): a zero-dimensional energy balance model of the Earth
- [tests/cases/positive/align-descriptions.cosi](tests/cases/positive/align-descriptions.cosi): the dimension of a value in SI units is inferred from its unit

**`to_SI` and `from_SI` conversions**
- [examples/demo/2-to-si.cosi](examples/demo/2-to-si.cosi): inputs in SI units are converted to dimensions with `from_SI`, passed to a law written with dimensions, and the result converted back to SI units (pascals) with `to_SI`

**Species and basis**
- [examples/demo/3-species.cosi](examples/demo/3-species.cosi): CO2 emissions in moles converted to kilograms (uncomment the last line to see the error from passing CH4 instead)
- [examples/diffusion.cosi](examples/diffusion.cosi): a diffusion kernel for the concentration of a species `X`
- [examples/quantity.cosi](examples/quantity.cosi) and [tests/cases/positive/basis-mult.cosi](tests/cases/positive/basis-mult.cosi): values with a `Basis`

**Open types**
- [examples/coriolis.cosi](examples/coriolis.cosi): imports units and declares a new kind of quantity, `sidereal_day`

**Affine spaces and coherence**
- [examples/demo/4-temperature.cosi](examples/demo/4-temperature.cosi): temperatures are points, and their differences (anomalies) are vectors
- [examples/affine.cosi](examples/affine.cosi): points, vectors, and getting back to a point from a weighted sum of points
- [examples/linear-interp.cosi](examples/linear-interp.cosi): a tutorial on linear interpolation, with scalars whose values are tracked in their types

**Polymorphism**
- [tests/cases/positive/poly-id.cosi](tests/cases/positive/poly-id.cosi): a polymorphic identity function, using a Python-style type parameter list and explicit type application `id[T]`
- [tests/cases/positive/poly-const.cosi](tests/cases/positive/poly-const.cosi): a function with two type parameters

**Tuples**
- [examples/tuples.cosi](examples/tuples.cosi): tuple types and destructuring

### REPL

Running `cosi` with no arguments starts the interactive REPL, which supports interactive evaluation and type inference. `cosi -i path/to/file` loads a file and then drops you into the REPL with its definitions in scope. In the REPL you can:

- **Evaluate an expression** — type any expression and press Enter
- **Infer a type** — `:t expr` infers the type of an expression; `:k type` infers the kind of a type
- **Load a file** — `:l path/to/file.cosi` runs type inference on the file and loads its definitions into the environment
- **Get help** — `:h` lists all available commands
- **Quit** — `:q`

**Example session:**

```bash
% cosi
...
> :l examples/units.cosi
Well-typed as Float[{UoM}][Unit[(m * s^-1)]]
0.25
units> x
1.0
units> :t x
Float[{UoM}][Unit[m]]
units> :k Float
{d : Descriptor} -> d -> Type
```

## Building and Installing

cosi is written in Haskell and is built with [Stack](https://docs.haskellstack.org/).

**Install (prebuilt binaries)**:

Download an archive for your platform from the [releases page](https://github.com/plas4sci/cosi/releases), unpack it, and put the `cosi` executable somewhere on your `PATH`. No Haskell toolchain required.

**Install (from source)**:

1. Install [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Clone this repo (`git clone https://github.com/plas4sci/cosi`)
3. `cd cosi`
3. Then run
```bash
stack install
```
(The first time it will install the GHC Haskell compiler).

After installing, `cosi` will be available as a command directly.

**For developers**:

Building:
```bash
stack build
```

Running the tests:
```bash
stack test
```


## Background

`cosi` is based on ideas around using 'graded monoids' (due to McBride and Nordvall-Forsberg) appearing in this paper:

* [Type systems for programs respecting dimensions (McBride, Conor, and Fredrik Nordvall-Forsberg); Advanced Mathematical and Computational Tools in Metrology and Testing XII. 2021. 331-345.](https://strathprints.strath.ac.uk/76626/1/McBride_etal_amctmtxii2021_type_systems_for_programs_respecting_dimensions.pdf)

and early ideas we had about building languages for science, in this paper:

* [A computational science agenda for programming language research (Orchard, Dominic, and Andrew Rice); Procedia Computer Science 29 (2014): 713-727.](https://www.cs.kent.ac.uk/people/staff/dao7/publ/iccs14-orchard-rice.pdf)

There are some similarities to [F#](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/units-of-measure) but cosi's aim is to be much more general and flexible, going beyond units-of-measure.

* You can see slides from a talk about these ideas that I (Dominic) gave in [November 2026 at ECMWF](https://drive.google.com/file/d/1oKRIpfI-6np-mkA-obT2AeEIo_ZGZcAp/view?usp=drive_link.).


## Contributing

This project is in its early stages, but we would welcome
any contributions, bug reports, or feature requests. Please start by raising an issue if you want to contribute or have a question.
