# Compile the tool

> make

# Translation

Assume your abc spec has name of "file.abc"

The command

> erl -eval "abc2umc:file(\"file.abc\")" -run init stop -noshell

will produce a file named "foo.umc" in m4 format

The next step is to use m4 command to produce an actual UMC model

> m4 foo.umc > filename.umc

Once the model is obtained, one can use UMC model checker for analysis

For use of UMC, please consult the website: http://fmtlab.isti.cnr.it/umc/V4.8/umc.html
