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

while(OS) --> ['while'], condition, { read_new_line(OS, end)}.
foreach(OS) --> ['foreach'], check_alphabet, ['('], chars, [')'], { read_new_line(OS, end) }.

%%%%%%%%%%%%%%%%%%%%
%Conditional statement
%%%%%%%%%%%%%%%%%%%%

condStatement(OS) --> ['if'], condition, ['then'], { read_new_line(OS, endif)}.
condStatement(OS) --> ['if'], condition, ['then'], { read_new_line(OS, else) }.
condStatement(OS) --> ['else'], ['if'], condition, ['then'], { read_new_line(OS, else) }.
condStatement(OS) --> ['else'], ['if'], condition, ['then'], { read_new_line(OS, endif) }.
condStatement(OS) --> ['else'], { read_new_line(OS, endif)}.


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
	atomic_list_concat(L,-,Attr),
	(L == X -> ! ;
	\+ X == end_of_line,
	phrase(statements(OS), X),
	read_new_line(OS, Attr)).

list_codes([], "").

list_codes([Atom], Codes) :- atom_codes(Atom, Codes).

list_codes([Atom|ListTail], Codes) :-
        atom_codes(Atom, AtomCodes),
    append(AtomCodes, ",", AtomCodesWithComma),
    append(AtomCodesWithComma, ListTailCodes, Codes),
    list_codes(ListTail, ListTailCodes).

list_string(List, String) :-
    ground(List),
    list_codes(List, Codes),
    atom_codes(String, Codes).

list_string(List, String) :-
    ground(String),
    atom_codes(String, Codes),
    list_codes(List, Codes).
