* the translator abc2abel deals with behaviour of the model vertex.abc which generates the module vertex.erl

* in order to complete the scenario, we need to provide some "hook" for initializing the input graph, and hence data for components

This is the purpose of user-defined modules: user_code.erl, dcolor.erl (to control the scenario) and readfile.erl (to read graph in DIMACS format)

