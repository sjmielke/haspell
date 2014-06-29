#Haspell
###Haskell spell correction based on minimum edit distance calculation
######Developed for a university course by Sebastian J. Mielke 2014

Haspell lets the user correct a text read form a file interactively by comparing words (using the minimum edit distance) to suggestions from a word list.

#Usage
    Haspell "wordlist.txt" "myfile.txt"
will read the file `wordlist.txt` and construct a Trie from the whitespace-separated words in this file. Then Haspell will go through the second file and present the user with correction suggestions for every word it cannot find in the given word list. The user can **i**gnore the spelling error, request **m**ore suggestions or e**x**it the program. Once all words are checked, the corrected text is written to `myfile.txt.corrected`.

###Additional Flags

CLI help is available using the usual `--help` flag, but let's talk about the available option here, regardless.

    Haspell "wordlist.txt" "myfile.txt" --compat
Usually the CLI will use ANSI terminal formatting. However, Windows users and users or less compatible terminals exist, so there is a `--compat` / `-c` option to turn off all formatting and instead highlight only the necessary parts of the UI using plain text cues.

    Haspell "wordlist.txt" "myfile.txt" --outfile "correctedfile.txt"
The `--outfile` / `-o` option allows the user to specify where the corrected output file should be saved to.

#Documentation

The code is documented using standard Haddock syntax, so I will only give a broad overview here.

The `WTrie` module defines the Trie data structure used to hold the word list as well as methods to import word lists from files and lists.
The usage of a Trie allows us to progressively calculate the minimum edit distance (MED) to a given user word without needing to re-calulate prefixes for every word.

The `TrieMED` module uses does precisely that. The `calcMEDs` entry point calls one of the `reduceTries` methods, which in turn traverse the Trie, carrying a state that contains mainly the current column of the MED matrix.
At each Trie node the `calcNode` method is called to transform the old state and the current node into the new state corresponding to the node.
The `step` function finally checks whether insertion, deletion, substitution or reversal are yield the smallest possible distance and fills the state accordingly.

The `Haspell` module finally contains the `main`. The `ArgParser` package provides the parsing of the command line arguments (see Usage).
After loading word lists, the `correctText` function calls `segmentText` to separate words from punctuation and then maps the monadic action `correctSentence` over the list of sentences. Here the `calcMEDs` method explained above is called, the results are printed and user input decides what the function returns. From the list of thus changed sentences the new corrected text can be created by `reconstructSentences` and then saved to a file.