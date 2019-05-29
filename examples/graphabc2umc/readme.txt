1. Compile the tool

> make

2. Translation

2.1. Assume your abc spec has name of "file.abc"

The command

> erl -eval "abc2umc:file(\"file.abc\")" -run init stop -noshell

will produce a file named "foo.umc" in m4 format

2.2. The next step is to use m4 command to produce the actual UMC model

> m4 foo.umc > filename.umc

2.3. Now the tool umc can be used to explore the model

> ./umc filename.umc

2.4. property checking

one can immediatly verify properties based on observable attributes

For example
a. AF FINAL
 --> check if the model contains a halting state

b. AF (FINAL and not has_attr(*,v))

---> all component do not have attribute attr with value v

or take a look at color.properties.txt file
