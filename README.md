Automated Theorem Prover for First-Order Logic

Input: 

	A text file containing one formula F.

Output: 

	If F is valid, VALID.
	If F is invalid, the program may terminate with INVALID or run forever.

Input formatting:

	Term Keywords:
		VAR[name]
			Variable called "name".
		OBJ[name]
			Object called "name".
		FUNC[name](terms)
			Function called "name". terms is a sequence of Terms separated by spaces.

	Formula Keywords:
		TRUE
			Logical true.
		FALSE
			Logical false.
		PRED[name](terms)
			Predicate called "name". terms is a sequence of Terms separated by spaces.
		AND(F G)
			Logical (F and G). F and G are Formulas.
		OR(F G)
			Logical (F or G). F and G are Formulas.
		NOT(F)
			Logical not(F). F is a Formula.
		IMPLIES(F G)
			Logical (F implies G). F and G are Formulas.
		IFF(F G)
			Logical (F iff G). F and G are Formulas.
		A{x}(F)
			For all x, F. x is a variable and F is a formula.
		E{x}(F)
			There exists an x such that F. x is a variable and F is a formula.

	Notes on syntax:
		Keywords are case sensitive.
		Names are strictly strings of uppercase and lowercase alphabet characters.
		Whitespace doesn't matter. Feel free to put newlines and spaces anywhere other than in the middle of a name or keyword string.
		If your input is not well-formed, the program will hopefully fail with some error message.

	Example input:
		IMPLIES( A{person}( PRED[areFriends](VAR[person] OBJ[Bob]) ) E{j}( PRED[areFriends]( VAR[j] OBJ[Bob] ) ) )

		This says "If everyone is friends with Bob, then someone is friends with Bob."

	Executing the program:
		An executable for x86_64 Windows systems is provided in the build folder. Using Command Prompt, navigate to \build and enter the following:
			ATP_x86_64_Win.exe [Input File Path]
		
		For other systems, the project needs to be compiled. We suggest using Cabal. From your shell, navigate to \AutomatedTheoremProver and enter the following to build and run:
			cabal run AutomatedTheoremProver -- [Input File Path]

 		Plenty of example input files can be found in \tests.
