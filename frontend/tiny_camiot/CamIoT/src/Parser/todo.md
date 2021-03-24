### Preprocessor
---
* Comment tokens are hardcoded, generalise
* There are some iffy things when you have trailing whitespaces and stuff after lines. Take a
  look at it and write some tests to make sure it does what you expect.
* Doublecheck that empty lines before and after the code section appears don't mess anything up.
  I recall one test made me scratch my head, but I can't reproduce it now.

### Parser
---
Go through all the files and import only what is needed, and hide the rest. It can take a while to compile things. (Does this actually speed up compilation? Ask @Abi or @Joel // Robert)