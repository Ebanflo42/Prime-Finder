# Prime-Finder
Program for finding primes written in Haskell.
The user is first asked whether or not they would like to use unlimited storage integers. If yes, a substantial number of primes are read from primeList.txt. If no, the user is asked whether or not they would like to load the prime list anyway. If no, the initial list is simply all single digit primes.
The command "List" prints the current list.
"List" followed by a number updates the list (searching for primes up to that number) and displays it.
"Bool" followed by a number updates the list in the same way and displays True or False depending on whether or not the number is in the resulting list.
"q" quits the program
In the future I would like to improve the navigation of this program - at the moment there is no "go back" command, only quitting. I would also like to be able to load an even larger prime list to begin with via http. Perhaps the finding of primes could be parallelized, but I'm not to sure about this.
