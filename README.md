
# Buttons that Solve Sudoku Puzzles

Github Pages Address for this repo: https://trequetrum.github.io/halogen-sudoku-solver/   
You can see the last deployment date by checking the last commit to the **/docs** folder.

---------

## Halogen Front-End: Nothing Fancy

This was conceived as a quickly hacked together front-end without much thought toward polish or a dialed in user experience. 

* Sometimes it's nice to have buttons for algorithms. 
* Try different inputs and watch how the output changes.

This project was initially a way to interactively try some of the algorithms written in my purescript Sudoku solver repository. That repository has since been deleted and merged into this project to form a mono-repo. This makes everything a bit easier to maintain as this project is no longer in active development.

## About The Halogen Front-End

The current UI is largely self-explanatory. It's worth mentioning that ctrl+clicking on an option for a cell in the Sudoku Board removes all other options for that cell. This is a helpful way to quickly enter your own puzzle to try things out. The reset button will only undo changes created by the various algorithms so you can use it as way to reset the board to the last user-created state.

## About The Sudoku Solver

### The Sudoku solver lives under `src/sudoku` and has two main components.

#### DSL, Types, and Common Transformations
The first component occupies the root of the folder and implements all the basic types and functions that interact with those types. These form a lightweight DSL to interact with Sudoku puzzles. These are:
* **Board**: Constructors, predicates, and a performant (FFI-based) board update mechanism. The only JavaScript code in the entire project lives here. 
* **Group**: Represents Rows, Columns, and Boxes for a sudoku board. Gives them a unique ID and is described by an array of indices that retrieve cells in a board.
* **Index**: Indices into a board are bounded the moment a board is created. A board has a fixed size and no holes. Here, indices are new-typed to allow for the construction of only valid indices into the Sudoku board. This gives us the performance benefit of never checking for index out of bounds errors and such. On the other hand, `Index.Internal` lets you bypass the smart constructor and break those guarantees in stunning ways (so be careful with that). 
* **Option**: Represents the symbols you fill out a Sudoku board with. 
* **OSet**: Represents a set of options. Cells are an index paired with an set of options, but they don't exist explicitly in this project. Instead, a board is implemented as an array of `OSet`s and cells are implicit in that relationship. While the board uses `OSet` to describe a Sudoku, strategies for solving Sudoku's often use them to represent the state-space of possible solutions.

#### Sudoku Solving Strategies

The second component is found in `src/sudoku/strategy` and implements a set of strategies for solving Sudoku and a bunch of utility functions that compose strategies together. 
* **Puzzle**: Puzzle is a type alias that augments a Sudoku board with meta-information. So, for example, the NTuple algorithm remembers which parts of the board it has previously searched and then doesn't search those regions again. The Brute Force strategy documents how many guesses it has to make before finding a solution. This could be implemented via a state monad. Though, in this case, the meta-information is specific to a board and not **just** the algorithm. For example, this information is displayed in the Halogen front-end and is as much an output of any given Sudoku strategy as the updated board-state. Because this "stateful" information is so closely linked to an already established data structure the state weaving is essentially done for free.
* **Stateful**: A functor that we can appropriate to add meta-information to the running of a strategy. So a strategy gets defined as `Puzzle -> Stateful Puzzle` (Though originally, it was `Board -> Stateful Board`). Everything that Stateful does for a strategy could be accomplished via meta-information in a puzzle (as described above), though the separation of concerns here is nice.

  The Stateful module has a function called `onlyAdvancing` that acts as a sort of restricted version of `bindFlipped` for `Stateful`. You can see the similarity in the type signatures if we could specialise bindFlipped for Stateful:

  ```purescript
  bindFlipped   :: ∀ a b. (a -> Stateful b) -> Stateful a -> Stateful b
  onlyAdvancing :: ∀ a  . (a -> Stateful a) -> Stateful a -> Stateful a
  ```

  This lets Stateful have a lot (but not all) of the power of a monad without actually being a monad. You can bind a stateful computation so long as its type is unchanged.
* **Strategy.Common**: This holds tasks that are likely to be common for all strategies. It can turn any strategy into one that checks if the updated board has been solved. It can turn any strategy into one that runs itself continuously until no more changes appear (The puzzle is stable). Stuff like that. Perhaps the most interesting is `ladderStrats` which combines an array of strategies into a single strategy. It does this by composing the strategies together, but starting again at the beginning if any of the strategies update the board. 
* **NTuples**: This is a strategy with it's own folder. [Here is a link](NTupleAlgorithm.md) that explains this strategy in some detail.
* **Pointing**: Pointing is when the state of one group can be used to propagate some constraints into the state of another. For example: If all the 2s in the second box happen to be in row 2, then the rest of row 2 cannot have any 2s. Most instances of pointing are actually covered by `NTuples` and the meta-information from `NTuples` can be used to reduce the state-space for pointing.
* **Bruteforce**: This is the only strategy that's guaranteed to solve a Sudoku puzzle as this does an exhaustive search for the solution. Thankfully, this can be augmented with a strategy for reducing the state space of the search. `norvigBruteForce`, for instance only looks for 1-tuples between guesses and is often faster than using some of the more expensive constraint-based strategies. `ladderBruteForce`, on the other hand uses all the available strategies to narrow the search space. While this is slower by up to a second on many Sudoku puzzles, it is considerably faster in the worst case. 

  Some numbers for just over 2,000,000 randomly generated puzzles:
  * About 1/5,000 puzzles take over 5,000ms for the norvigBruteForce to solve (One took just under 15minutes!)
  * Removing outliers, norvigBruteForce averages 53ms per puzzle.
  * The slowest ladderBruteForce puzzle took 4,721 seconds to solve.
  * Removing outliers, ladderBruteForce averages 290ms per puzzle.


## Building for Github Pages

./build-gh-pages is a bash script that deploys this halogen-based SPA into **/docs** and formats relative links so that they play nice with Github pages.

------

# Halogen Template

### Quick Start
```sh
git clone https://github.com/purescript-halogen/purescript-halogen-template.git halogen-project
cd halogen-project
npm install
npm run build
npm run serve
```

### Introduction

This is a template for starting a fresh project with the [Halogen](https://github.com/purescript-halogen/purescript-halogen) library for writing declarative, type-safe user interfaces.

You can learn more about Halogen with these resources:

- The [Halogen documentation](https://github.com/purescript-halogen/purescript-halogen/tree/master/docs), which includes a quick start guide and a concepts reference.
- The [Learn Halogen](https://github.com/jordanmartinez/learn-halogen) learning repository.
- The [Real World Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld) application and guide. Note that the published article is written for the older halogen v4, but the code and comments cover the current halogen v5.
- The [API documentation](https://pursuit.purescript.org/packages/purescript-halogen) on Pursuit

You can chat with other Halogen users on the [PureScript Discourse](https://discourse.purescript.org), or join the [Functional Programming Slack](https://functionalprogramming.slack.com) ([invite link](https://fpchat-invite.herokuapp.com/)) in the `#purescript` and `#purescript-beginners` channels.

If you notice any problems with the below setup instructions, or have suggestions on how to make the new-user experience any smoother, please create an issue or pull-request.

Compatible with PureScript compiler 13.x

### Initial Setup

**Prerequisites:** This template assumes you already have Git and Node.js installed with `npm` somewhere on your path.

First, clone the repository and step into it:

```sh
git clone https://github.com/purescript-halogen/purescript-halogen-template.git halogen-project
cd halogen-project
```

Then, install the PureScript compiler, the [Spago](https://github.com/purescript/spago) package manager and build tool, and the [Parcel](https://github.com/parcel-bundler/parcel) bundler. You may either install PureScript tooling _globally_, to reduce duplicated `node_modules` across projects, or _locally_, so that each project uses specific versions of the tools.

To install the toolchain globally:
```sh
npm install -g purescript spago parcel
```

To install the toolchain locally (reads `devDependencies` from `package.json`):
```sh
npm install
```

### Building

You can now build the PureScript source code with:

```sh
# An alias for `spago build`
npm run build
```

### Launching the App

You can launch your app in the browser with:

```sh
# An alias for `parcel dev/index.html --out-dir dev-dist --open`
npm run serve
```

### Development Cycle

If you're using an [editor](https://github.com/purescript/documentation/blob/master/ecosystem/Editor-and-tool-support.md#editors) that supports [`purs ide`](https://github.com/purescript/purescript/tree/master/psc-ide) or are running [`pscid`](https://github.com/kRITZCREEK/pscid), you simply need to keep the previous `npm run serve` command running in a terminal. Any save to a file will trigger an incremental recompilation, rebundle, and web page refresh, so you can immediately see your changes.

If your workflow does not support automatic recompilation, then you will need to manually re-run `npm run build`. Even with automatic recompilation, a manual rebuild is occasionally required, such as when you add, remove, or modify module names, or notice any other unexpected behavior.

### Production

When you are ready to create a minified bundle for deployment, run the following command:
```sh
npm run build-prod
```

Parcel output appears in the `./dist/` directory.

You can test the production output locally with a tool like [`http-server`](https://github.com/http-party/http-server#installation). It seems that `parcel` should also be able to accomplish this, but it unfortunately will only serve development builds locally.
```sh
npm install -g http-server
http-server dist -o
```

If everything looks good, you can then upload the contents of `dist` to your preferred static hosting service.
