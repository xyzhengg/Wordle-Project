# Wordle-Project
[To play the game, click here](https://xyzhengg.github.io/Wordle-Project/)
5 letter word
- need to be able to check letter position if correct or not
    - if correct, append the letter to the whole column where it belongs and highlight green
    - if correct but wrong position, turn yellow
    - if wrong, turn grey
- last row: make the boxes bolder so it's obvious that's the final output
- only get 5 guesses!!!

## Functionality
1. Search through the word array provided to make the checks
2. Make it work with keyboard inputs, as well as on screen keyboard
3. Maybe have a restart button? But have to also make winning streak go back to 0
4. Let person change letter typed after they chose a letter ONLY BEFORE they enter the whole word
5. Must press a 'check' button to submit and check the word they chose
6. Automatically move on the to next box after tying into one box
7. After winning or losing, ot play again, there will be a "play again" button flashing

## Optional Functionality
5. Could be fun if they win, make fireworks go off in the background (only if time allows)
6. Add sound on button presses etc?
7. Be able to do language adaptations and select what language to use
    e.g. german, french, latin, italian, dutch
    e.g. american vs british spelling
8. Could also make it allow more or less guesses!

## Keeping Track of Things
1. If win, tell em they won, and can have another go
    - if possible each time they win, add to Winning Streak Counter
        - might need some kind of thing/cookies? or some temp storage of the win

2. If lose, tell em they lose and can try again
    - make winning streak go back to 0

3. Can maybe keep track of how many guesses it takes to win each time?