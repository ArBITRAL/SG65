
Here you can find the biunary executables for
macOS, Windows, Ubuntu Linux,  of the command-line version
of the UMC model checker (and related utilities).

UMC can be called in thje following way:

umc  [options]  modelfilename                    -- interactive versions
umc  [options]  modelfilename formulafilename    -- batch version

The most important options from the point of vire of the user are the following:
-s => silent, does not print progressing messages during evaluation
-<n> => sets Default_LTS_Depth to n, for n=1 => breadth first like evaluation
+<n> => sets Depth_Increment to n
-x => sets Static_Max_Depth
-z => sets NoExplanations
+z => sets AutoExplanations

Examples:

umc -100000 -z  modelfilename formulafilename
-- This is the most efficient way of verifying a formula:
--  the evaluation is depth first, not interactive, no explanation-dependent data saved
--  during the evaluation.

umc   -100000 modelfilename 
--  Interactive depth first verification, once a formula is verified, the explanation 
-- can be requested, and the model can be explored to observe the internal structure of the
-- various states (that includes the values of the subformulas in that state, for 
-- which can re requested specific subexplanations)

umc   -16 modelfilename 
--  Same as above, only that the evalution precedes in a breadth first like way.
--  doubling the maximum depth of analysis at each step)