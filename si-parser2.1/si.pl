%%%%%%%%%%%%%%%%%%%%
%INIT
%%%%%%%%%%%%%%%%%%%%

prog:-
	see('input-csh'),
	open('output-bash', append, OS),
	read_new_line(OS),
	close(OS).

prog(Filename):-
	see(Filename),
	open('output-bash', append, OS),
	read_new_line(OS),
	close(OS).

%%%%%%%%%%%%%%%%%%%%
%SK£ADNIA CSH
%%%%%%%%%%%%%%%%%%%%

statements(OS) --> functions(OS), !.
statements(OS) --> loops(OS), !.
statements(OS) --> condStatements(OS), !.

functions(OS) --> echo(OS), !.
functions(OS) --> set(OS), !.

loops(OS) --> while(OS), !.
loops(OS) --> foreach(OS), !.

condStatements(OS) --> condStatement(OS), !.



%%%%%%%%%%%%%%%%%%%%
%FUNCTIONS
%%%%%%%%%%%%%%%%%%%%

echo(OS) --> [echo], chars, !.
echo(OS) --> [echo].

set(OS) --> [set],  variable, [=], chars, !.



%%%%%%%%%%%%%%%%%%%%
%LOOPS
%%%%%%%%%%%%%%%%%%%%

while(OS) --> ['while'], condition, { read_new_line(OS, [end])}.
foreach(OS) --> ['foreach'], check_alphabet, ['('], chars, [')'], { read_new_line(OS, [end]) }.

finals(X, OS) --> ['end'].
finals(X, OS) --> ['endif'].
finals([H|X], OS) --> ['else'], condStatement(OS).



%%%%%%%%%%%%%%%%%%%%
%Conditional statement
%%%%%%%%%%%%%%%%%%%%

condStatement(OS) --> ['if'], condition, ['then'], { read_new_line(OS, [else, endif])}.




%%%%%%%%%%%%%%%%%%%%
%UTILS
%%%%%%%%%%%%%%%%%%%%

variable --> ['$'], check_alphabet.
variable --> check_alphabet.

condition -->  ['('], variable, condition_sign, check_number_alphabet, [')'].
condition -->  ['('], variable, condition_sign, variable, [')'].

condition_sign --> ['<'].
condition_sign --> ['<'], ['='].
condition_sign --> ['>'].
condition_sign --> ['<'], ['='].
condition_sign --> ['='],['='].

chars --> [_], chars.
chars --> [].

check_alphabet --> [X], { atom_chars(X, [H|T]), char_type(H, alpha), check_number_alphabet(T)}.
check_number_alphabet --> [X], { atom_chars(X, L), check_number_alphabet(L) }.

check_number_alphabet([H|T]):- char_type(H, alnum), check_number_alphabet(T).
check_number_alphabet([]).

atom_is_alphabet([_,N|T]) :-
    atom_chars(N, [L]),
    char_type(L, alpha).

atom_is_alphabet(N) :-
    atom_chars(N, [L]),
    char_type(L, alpha).

read_new_line(OS) :-
	readln(X),
	\+ X == end_of_line,
	writeln(X),
	phrase(statements(OS), X).

read_new_line(OS, Attr) :-
	repeat,
	readln(X),
	writeln(X),
	%atomic_list_concat(L,-,Attr),
	(check_list_attr(Attr, X, X, OS) -> ! ;
	\+ X == end_of_line,
	phrase(statements(OS), X),
	read_new_line(OS, Attr)).


check_list_attr([HA|TA], [HP|TP], [HPO|TPO],OS):-
	(HA == HP -> phrase(finals([HPO|TPO], OS), [HPO|TPO]), !;
		(\+ TP == [] -> check_list_attr([HA|TA], TP, [HPO|TPO], OS);
		(\+ TA == [] -> check_list_attr(TA, [HPO|TPO], [HPO|TPO], OS); !, fail))).
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	