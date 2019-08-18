# Interval_analysis
Interval Analysis implementation for abstract language using [OCaml](https://ocaml.org/). This project is completed as a part of practice in JetBrains Lab.

### Getting started

Type 'make' to build project. 

Example of use:

```
./prog test.t [OPTION] [list of integers]
```

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
	
