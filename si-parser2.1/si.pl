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

echo(OS) --> [echo], (null -> {write(OS, 'echo ')}, chars(OS), !; {write(OS, 'echo'), nl(OS)}, !).

set(OS) --> [set], variable(X), [=], chars(OS), !, {write(OS, X), nl(OS)}.



%%%%%%%%%%%%%%%%%%%%
%LOOPS
%%%%%%%%%%%%%%%%%%%%

while(OS) --> ['while'], condition(OS), { read_new_line(OS, [end])}.
foreach(OS) --> ['foreach'], check_alphabet(X), ['('], chars(OS), [')'], { read_new_line(OS, [end]) }.

finals(X, OS) --> ['end'], {write(OS, 'done'), nl(OS)}.
finals(X, OS) --> ['endif'], {write(OS, 'fi'), nl(OS)}.
finals([H|X], OS) --> ['else'], {write(OS, 'else'), nl(OS)}, condStatement(OS).



%%%%%%%%%%%%%%%%%%%%
%Conditional statement
%%%%%%%%%%%%%%%%%%%%

condStatement(OS) --> ['if'], condition(OS), ['then'], { read_new_line(OS, [else, endif])}.



%%%%%%%%%%%%%%%%%%%%
%UTILS
%%%%%%%%%%%%%%%%%%%%

variable(X) --> ['$'], {append(X, ['$'], X1)}, check_alphabet(X2), {X = [X1|X2]}. %wtf
variable(X) --> check_alphabet(X).
%variable --> [X], {write(X)},check_alphabet.

condition(OS) -->  ['('], variable, condition_sign(OS), (check_number_alphabet -> [')']; variable, [')']).
%condition(OS) -->  ['('], variable, condition_sign(OS), variable, [')'].

condition_sign(OS) --> ['<'], {write(OS, '-lt'), nl(OS)}.
condition_sign(OS) --> ['<'], ['='], {write(OS, '-le'), nl(OS)}.
condition_sign(OS) --> ['>'], {write(OS, '-gt'), nl(OS)}.
condition_sign(OS) --> ['<'], ['='], {write(OS, '-ge'), nl(OS)}.
condition_sign(OS) --> ['='],['='], {write(OS, '-eq'), nl(OS)}.

chars(OS) --> [X], {write(OS, X)}, chars(OS).
chars(OS) --> [], {nl(OS)}.
null --> [].

check_alphabet(X) --> [X], { atom_chars(X, [H|T]), char_type(H, alpha), check_number_alphabet(T)}.
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

read_new_line(OS, Attr):-
	readln(X),
	writeln(X),
	(check_list_attr(Attr, X, X, OS) -> ! ;
	phrase(statements(OS), X),
	read_new_line(OS, Attr)).

check_list_attr([HA|TA], [HP|TP], [HPO|TPO],OS):-
	(HA == HP -> phrase(finals([HPO|TPO], OS), [HPO|TPO]), !;
		(\+ TP == [] -> check_list_attr([HA|TA], TP, [HPO|TPO], OS);
		(\+ TA == [] -> check_list_attr(TA, [HPO|TPO], [HPO|TPO], OS); !, fail))).