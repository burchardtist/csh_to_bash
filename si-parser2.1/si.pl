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
	read_new_line(OS).
	

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

set(OS) --> [set], variable1(X), [=], chars1([],X1), !, {atomic_list_concat(X, '', X3), atomic_list_concat(X1, '', X4), write(OS, X3), write(OS, '='), write(OS, X4), nl(OS)}.



%%%%%%%%%%%%%%%%%%%%
%LOOPS
%%%%%%%%%%%%%%%%%%%%

while(OS) --> ['while'], {write(OS, 'while ')}, condition(OS), { read_new_line(OS, [end])}.
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

variable(X) --> ['$'], check_alphabet(X2), {X = ['$', X2]}.
variable(X) --> check_alphabet(X).
variable1(X) --> ['$'], check_alphabet(X2), {X = ['$', X2]}.
variable1(X) --> check_alphabet(X1), {X = [X1]}.

condition(OS) -->  ['('], variable(X), condition_sign(X11), {atomic_list_concat(X, '',Z), atomic_list_concat(X11, ' ', Z11) }, (check_number_alphabet(X4) -> 
																				[')'], {atomic_list_concat(X4, '', Z4), write(OS, '( '), write(OS, Z), write(OS, ' '), write(OS, Z11), write(OS, ' '), write(OS, Z4), write(OS, ' )'), nl(OS)};
																				variable(X1), [')'], {atomic_list_concat(X1, '', Z1), write(OS, '( '), write(OS, Z1), write(OS, ' )'), nl(OS)}).

condition_sign(X) --> ['<'], {X = ['-lt']}.
condition_sign(X) --> ['<'], ['='], {X = ['-le']}.
condition_sign(X) --> ['>'], {X = ['-gt']}.
condition_sign(X) --> ['<'], ['='], {X = ['-ge']}.
condition_sign(X) --> ['='],['='], {X = ['-eq']}.

chars(OS) --> [X], {write(OS, X)}, chars(OS).
chars(OS) --> [], {nl(OS)}.
chars1(T,L) --> [X], ({T == []} 
						-> chars1([X],L)
						; {append(T,[X],Z)}, chars1(Z,L)
					 ).
chars1(T,L) --> [], {L = T}.

null --> [].

check_alphabet(X) --> [X], { atom_chars(X, [H|T]), char_type(H, alpha), check_number_alphabet(T)}.
check_number_alphabet(X1) --> [X], { atom_chars(X, L), check_number_alphabet(L), X1 = L }.

check_number_alphabet([H|T]):- char_type(H, alnum), check_number_alphabet(T).
check_number_alphabet([]).

atom_is_alphabet([_,N|T]) :-
    atom_chars(N, [L]),
    char_type(L, alpha).

atom_is_alphabet(N) :-
    atom_chars(N, [L]),
    char_type(L, alpha).

read_new_line(OS) :-
	readln(X, EOL),
	( EOL == end_of_file
		-> !,   ( X == [] 
					-> close(OS)
					; writeln(X), phrase(statements(OS), X), close(OS)
				)
		; writeln(X),
		phrase(statements(OS), X), read_new_line(OS)
	).

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