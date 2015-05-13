Interval_analysis

Try 'make' to build project. 

Example of use:

./prog test.t [OPTION] [list of integers]

OPTION:

	-trace	  - trace instruction's list
	-interval - run interval analysis
	-eval 	  - run interp.
	
list of integers:

	value for 'read x;' instr.
	

ATTENTION: 

	you should follow the rules in case of using -interval option 
	
RULES:

	- NO &&,|| operators
	- format of logical expression 
		VAR op AE, where VAR - variable, 
		op - logical operator, AE - arithmetic expression
	- NO RECURSION
	
