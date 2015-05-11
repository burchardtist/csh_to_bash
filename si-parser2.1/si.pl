%%%%%%%%%%%%%%%%%%%%
%INIT
%%%%%%%%%%%%%%%%%%%%

prog:-
	see('hello'),	
	read_new_line.
	
	
	
%%%%%%%%%%%%%%%%%%%%
%SK£ADNIA CSH
%%%%%%%%%%%%%%%%%%%%
		
statements(X) --> functions(X), !.
statements(X) --> loops(X), !.

functions(X) --> echo, !.
functions(X) --> set, !.

loops(X) --> while, !.



%%%%%%%%%%%%%%%%%%%%
%FUNCTIONS
%%%%%%%%%%%%%%%%%%%%

echo --> [echo], chars, !.
echo --> [echo].

set --> [set],  variable, [=], chars, !.



%%%%%%%%%%%%%%%%%%%%
%LOOPS
%%%%%%%%%%%%%%%%%%%%

while --> ['while'], condition, { read_new_line(end) }.	
	
	

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
	
read_new_line :- 	
	readln(X),
	\+ X == end_of_line,
	writeln(X),
	phrase(statements(X), X).
	
read_new_line(Attr) :-
	repeat,
	readln(X),
	writeln(X),
	atomic_list_concat(L,-,Attr),
	(L == X -> ! ;
	\+ X == end_of_line,	
	phrase(statements(X), X),
	read_new_line(Attr)).