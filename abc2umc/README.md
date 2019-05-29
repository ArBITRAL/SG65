# Compile the tool

> make

# Translation

Assume your abc spec has name of "file.abc"

The command

> erl -eval "abc2umc:file(\"file.abc\")" -run init stop -noshell

will produce a file named "foo.umc" in m4 format

The next step is to use m4 command to produce an actual UMC model

> m4 foo.umc > filename.umc

# Once the model is obtained, the next step is to use UMC model checker for analysis
