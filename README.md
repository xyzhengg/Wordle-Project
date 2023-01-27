# Wordle-Project
To play the game, [click here](https://xyzhengg.github.io/Wordle-Project/)

5 letter word
- need to be able to check letter position if correct or not
    - if correct, append the letter to the whole column where it belongs and highlight green
    - if correct but wrong position, turn yellow
    - if wrong, turn grey
- only get 6 guesses!!!

## Functionality
1. Search through the word array provided to make the checks
2. Make it work with keyboard inputs, as well as on screen keyboard
8. Need to show the answer if they lose
3. Maybe have a restart button? But have to also make winning streak go back to 0
4. Let person change letter typed after they chose a letter ONLY BEFORE they submit the whole word
5. Must press a button to submit and check the word they chose
6. Automatically move on the to next box after tying into one box
7. After winning or losing, to play again, there will be a "play again" button flashing

## Extra Functions
1. Add sound on button presses etc?
2. DONE Maybe a scoring system?
3. Resize screen for smaller devices
4. DONE Special word "Xin Yu"
5. DONE Flip animation for "Xin Yu" guess
6. Keyboard typing works

# Game Plan/ Progress
1. DONE Function for coming up with the random word
2. DONE Make keyboard inputs and screen keyboard work and type into the boxes
3. DONE Make the wordle grid function the way I want
    - click anywhere on the grid, and any typing will auto start in the first row
    - after first row has been SUBMITTED, any clicking will auto go onto the second row etc etc.
4. DONE Make a function to go through persons input and compare to the random word I generated. . . 
    - style colors on wordle grid based on letter position
5. DONE Make a win function with streak counter
6. DONE Make a lose function than zeros streak counter
    - two parameters: last box is not empty, and word inputted is NOT empty
7. DONE If lose or win, allow player to play again. If yes, generate new word
8. DONE Create reset button? (can maybe just refresh the page/reload window)
9. DONE Maybe can introduce point system
10. DONE Make modal for winning and losing with play again button
11. DONE Make modal for instructions
12. DONE Add special word xinyu for extra points
    - only allowed once per game
13. DONE Reposition winner, loser, etc in modal
14. DONE Reposition xin yu in modal and style associated functions
14. DONE Add +score to play again modal
14. DONE Pulse on-screen play again button and make it orange
15. DONE Update instructions modal with pictures and instructions on points system
16. Change alert word not real to a modal
14. DONE Delay for winner and loser and xin yu functions
14. DONE Flip tiles animation for special "Xin Yu" word
15. Sound?
16. Keyboard function?
17. Make it fit to different screen sizes?
