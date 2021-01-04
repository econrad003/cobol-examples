# cobol-examples
Some examples of code written in COBOL

Group 1: Solving triangles
+ *sss.cob* - SSS configuration - given the length of its three sides, solve the triangle using the Law of Cosines.  This example uses the intrinsic function *acos* (arccos - inverse cosine) and the intrinsic constant pi.  NB: Complaints about the (restricted!) "GO TO" used in validating the input will be ignored.  (It would not be difficult to eliminate by adding a flag and changing the PERFORM statements to PERFORM UNTIL, but maintaining the flag makes the resulting code slightly more complex.  Code could be simplified using CALLed subprograms.)
