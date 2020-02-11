# FIT2102 Assignment 2: Hearts

 - **Due Date**: TBD
 - **Weighting**: 20% of your final mark for the unit
 - **Uploader**: <https://fit2102.monash/uploader/>

## Hearts

The game of Hearts[^1] is a trick-taking game where the goal to score as *few
points* as possible.  All cards in the Hearts are worth one point, and the
Queen of Spade is worth 13.  Your task will be to implement a simple AI to beat
other players (students and staff) at this game.

There will be two versions of the game:

 1. A two-player version, which will be used to assess the *strength* of your
    player.  This version will use calibrated opponents and can be seen as a
    test.
 2. An online tournament where your player will be pitted against other AIs in
    four-player matches.

To play the game with your AI, you will need to implement a function called
`playFunc` which will be given a number of parameters upon which you will need
to act and return the card you believe will lead to victory.  An important
component of the game setup is that you will be allowed to have a form of
*memory*.

Memory, in this case, will be entirely determined by you.  The game will simply
pass you information pertaining to the current round: your hand and other cards
played; and the previous round: the complete set of cards played at the
previous round and your memory.

The game you will play will follow most of the classic rules of Hearts.  This
game is very popular as it is easy to lean and play, thus many online versions
are available if you want to train.  Do note, though, that most of them usually
vary a little, so take care to read the rules carefully.

At the core of the game of Hearts are the *point cards*: all cards in Hearts
and the Queen of Spade.  The goal of the game is to score as few points as
possible.  An alternative exists though: if a player manages to take all point
cards, she gets 0 points and *every other player* gets 26 points (the maximum).

At the end of each *round*, when players have played all the cards in their
hands, we tally the points in every trick taken by each player.  That is, we
sum the point cards they won.

The game goes on until at least one player has scored more
than 100 points *and* there is exactly one player with the lowest score.  The
player with the lowest score is the winner.

Counting points in Hearts is done as follows:

 1. **Point cards:** Each Hearts is worth 1 point, the Queen of Spades is worth
    13 points.
 2. **Hand:** At the end of each hand (players have played all their cards),
    each player receives the amount of points in the tricks they won.
 3. **Game:** The game stops when a player has reached 100 points and exactly
    one other has the lowest score; the latter is the winner.

The rules of Hearts we will use are as follows:

 1. **Reneging:** Each turn starts with a player playing a single card, the
    suit of which determines the suit of the trick.  Other players must *follow
    suit* (play cards in the same suit) if they are able to; if they do not
    have any cards in that suit, they may discard any card of their choosing.
 2. **Bleeding:** No point card may be played during the first round unless the
    player has no other choice.
 3. **Leading:** The player with the Two of Clubs leads (starts) the first
    round with this card.
 4. **Breaking:** No player may start a trick with a Heart before any has been
    played, unless they have no other choice.
 5. **Shooting the Moon:** In case a player took all the point cards, she
    scores 0 points and the other players all score 26 points.

## Deliverable

For this assignment, you are required to implement a function `PlayFunc`
defined as:

```haskell
type PlayFunc
  =  PlayerId -- ^ this player's Id so they can identify themselves in the bids
              -- and tricks
  -> [Card]   -- ^ the player's cards
  -> Trick    -- ^ cards in the current trick, so far
  -> Maybe ([(Card, PlayerId)], String) -- ^ previous player's state
  -> (Card, String)             -- ^ the player's chosen card and new state
```

So, you need to write a function that takes:

 - Your id so you can reference you player in the data.
 - The cards in your hand.
 - The cards played by other players in this round.
 - The previous (complete) trick and your *state*.

This function must return: the card you chose, and your new state.  This state
can be *any type you want*.  In its most simple form, you are free to use a
`String` to represent your memory.  Although using a more complex datatype will
lead to a higher mark in the Code Quality.  However, if you do implement a
custom type it must have a `Read` and a `Show` instance.  This is necessary for
us to save and pass around the data you saved.

Before uploading your player, please check that the following run:

```
stack exec staticgame
```

This will run a single game with four instances of your player.

```
stack test
```

This will make sure your functions respect the bidding and playing
rules. If your code does not pass the tests, you will not be able to
access the tournament.

The code provided uses the Safe pragma[^2] to make sure the code you
use is okay to run. It is also compiled with the `-Werror` flag which
means that all warnings will throw errors and prevent your code from
compiling. So do make sure you run the test suite before you upload
your player.

## Assessment

The assessment for this assignment will be in three parts:

 1. **Documentation and comments (30 marks)**
 2. **Code quality (40 marks)**
 3. **Performance (30 marks)**

### Documentation and comments

You are required to provide a *file-level comment*, *function comments* and
*line comments* where necessary.  The most important part of commenting is to
provide information, not to describe the code.

 - **File-level doc:** You should provide an *overview* of your code and
   explain your rationale (how you decided to play) in a block comment at the
   top of the file.  Think of this as a technical report: you want to summarise
   the workings of the code and highlight the interesting parts.
 - **Function comments:** These comments should be situated above the type
   declaration of the function they refer to.  They should give the *manual* of
   the function, explaining how it works -- and not simply describing what each
   line/function does.
 - **Line comments**: In some cases, your code might be hard to read, or the
   action you do is counter-intuitive; in those cases, you can add a line
   comment to add information.

### Code quality

The code quality will be the main evaluation criterion for your assignment.
Your code needs to respect the Style Guide (TODO) and use, efficiently,
concepts from the course.  You can think of this as a two-part marking scheme:

 1. Apply concepts from the course.  The important thing here is that you need
    to actually use them somewhere.  For example, defining a new type and its
    Monad instance, but then never actually needing to use it will not give you
    marks.  (Note: using bind `(>>=)` for the sake of *using the Monad* when it
    is not needed will not count as "efficient usage.")
 2. Have general code.  Because of the tournament format, you should try to
    have code that can deal with two- and four-player games using the same
    functions instead of writing two separate blocks of code.

### Performance

The important thing in this assignment is: *your rank in the tournament will
not influence your mark.* The performance of your player will be evaluated
automatically by the test suite.  You get 10 marks for defeating each:

 - The basic player, only tries to play a valid move.
 - The minmax player, which uses an simple implementation of the min-max
   algorithm.
 - The advanced player, which will use a more advanced search strategy.

### Marking rubric

 - **Pass:** The code compiles and your players respects the rules of the game,
   you are able to beat the *minmax player* and use some form of memory.
 - **Credit:** The code compiles without warnings and always return a valid
   card.  You use the memory to store non-trivial information.
 - **Distinction:** The code is well structured, efficient, and uses some
   advanced concepts from the course (higher order functions, function
   composition, monadic operations, etc.)
 - **High Distinction:** The code does not contain any excess parts, the player
   can defeat all training opponents, and the commenting supports the
   submission.

Do note you can expect a higher mark with an average level AI with very neat
code, rather than a high-performing AI with spaghetti code.

Outstanding submissions, common mistakes and complete disasters may be
discussed in class with your name removed.

### Plagiarism

We will be checking your code against the rest of the class, and the
internet, using a plagiarism checker. Monash applies strict penalties
to students who are found to have committed plagiarism.

Any plagiarism will lead to a 0 mark for the assignment and will be
investigated by the staff.  There is a zero-tolerance-policy in place at
Monash.  So be careful and report any collaboration with other students.

## Tournament



We will run a tournament online based on the code provided. Except the
interface, this will be the same game. We run a server for the course
at <https://fit2102.monash> with the following pages:

**Important**: Your rank in the tournament will not have a direct
impact on your mark. A high-performing player with spaghetti code will
be graded lower than an average, well-written player.

 - [The uploader](https://fit2102.monash/uploader/): after logging in,
   this page will allow you to upload your code and compete in the
   tournament.
 - [The handout](https://fit2102.monash/resources/assignment.html):
   this document.
 - [The ladder](https://fit2102.monash/ladder.php): this
   page will display the scores of the last tournament run.
 - [The docs](https://fit2102.monash/docs/): documentation about the
   assignmentâ€™s code.

Once you upload your player, you will see two links on the page:

 - `home.php`: shows your current ranking, last upload, and previous
   games played.
 - `status.php`: shows the status of your current upload.

Furthermore, you can inspect your games by clicking on their number.

## Game AI

The goal of this assignment is not for you to develop an AI which can compete
with openAI or AlphaGo.  The emphasis should be on code quality and applying
functional concepts.  Below, you can find a list of *basic* AI algorithms,
ranked by implementation difficulty.

 - **Naïve AI:** tries to play its best card given the current state of the
   game, you can start by implementing one to make sure you respect the game's
   rules.
 - **Heuristic player:** will save additional information about the game being
   played to enhance its decision at each turn.
 - **MinMax:[^4]** tries to minimise the maximum loss of a player by building a
   tree of possible moves and playing against itself.  This method was
   developped for two-player, zero-sum game with perfect information.  In this
   context, it will be useful in the two-player version but will require
   modification in the four-player context.
 - **Probabilistic player:** will make use of probabilities to determine which
   cards have the highest chance of winning the game (not the trick).
 - **Monte Carlo Tree Search:** is the fusion between search algorithms such as
   minmax and using probabilities to determine the branching in the search
   tree.  Will also make use of a *simulation* phase to explore deeper.

[^1]: https://en.wikipedia.org/wiki/Hearts_(card_game)

[^2]: More info at [SafeHaskell](https://ghc.haskell.org/trac/ghc/wiki/SafeHaskell), but this should not hinder your work.

[^4]: https://en.wikipedia.org/wiki/Minimax
