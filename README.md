# Prime-Finder
Program for finding primes written in Haskell.

The user is first asked whether or not they would like to use unlimited storage integers. 
If yes, a substantial number of primes are read from primeList.txt. 
If no, the user is asked whether or not they would like to load the prime list anyway. 
If no, the initial list is simply all single digit primes.

The command "list" prints the current list.
"list" followed by a number updates the list (searching for primes up to that number) and displays it. "head" does the same thing except it only displays the head (greatest value) of the list.
"bool" followed by a number updates the list in the same way and displays True or False depending on whether or not the number is in the resulting list.
"q" quits the program. "h" displays help.

I would like to be able to load an even larger prime list to begin with perhaps via http. 
Perhaps the process of finding primes could be parallelized, but I'm not to sure about this.
