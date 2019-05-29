### The folder generated code contains an example of the direct output produced by abc2umc from vertex.abc

As noted in the corresponding tool folder, this output should be further processed by m4 macro)


### The folder models contains all UMC models for graphs with the number of vertices from 2 to 5
### The file vertex2orginial.umc is the orignial specification which contains a bug where a counter example can be obtained from verifying property G2

1. Using umc to explore the model, assuming filename.umc

> ./umc filename.umc

2. property checking

We can verify properties based on "observable" attributes, declared in
the section Abstractions of the model.

For example
a. AF FINAL
 --> check if the model contains a halting state

b. AF (FINAL and not has_attr(*,v))

---> all component do not have attribute attr with value v

For the graph colouring example, please take a look at color.properties.txt file

For use of UMC, please consult the website: http://fmtlab.isti.cnr.it/umc/V4.8/umc.html
