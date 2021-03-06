%%%%%%%%%%%%%%%%%%%%
%INIT
%%%%%%%%%%%%%%%%%%%%

prog:-
	see('input-csh'),
	open('output-bash', append, OS),
	read_new_line(OS).

prog(Filename):-
	see(Filename),
	open('output-bash', append, OS),
	read_new_line(OS).


%%%%%%%%%%%%%%%%%%%%
%SK�ADNIA CSH
%%%%%%%%%%%%%%%%%%%%

statements(OS) --> functions(OS), !.
statements(OS) --> loops(OS), !.
statements(OS) --> condStatements(OS), !.
statements(OS) --> [X], ({X == []} 
	-> {nl(OS)}, !
	;{write(OS, X)}, statements(OS), !).
statements(OS) --> nullline(OS), !.

nullline(OS) --> [], {nl(OS)}, !.


functions(OS) --> echo(OS), !.
functions(OS) --> set(OS), !.
functions(OS) --> cd(OS), !.
functions(OS) --> mkdir(OS), !.
functions(OS) --> rm(OS), !.
functions(OS) --> rmdir(OS), !.
functions(OS) --> cat(OS), !.
functions(OS) --> ps(OS), !.

loops(OS) --> while(OS), !.
loops(OS) --> foreach(OS), !.

condStatements(OS) --> condStatement(OS, 0), !.

%%%%%%%%%%%%%%%%%%%%
%FUNCTIONS
%%%%%%%%%%%%%%%%%%%%

echo(OS) --> [echo], (null 
							-> {write(OS, 'echo ')}, streamOrChars(OS), !
							; {write(OS, 'echo'), nl(OS)}, !).
echo(OS) --> [echo], {write(OS, 'echo ')}.

cat(OS) --> [cat], {write(OS, 'cat ')}, streamOrChars(OS), !.

set(OS) --> [set], variable1(X), [=], ['('], chars1([],X1), !, {atomic_list_concat(X, ' ', X3), atomic_list_concat(X1, ' ', X4),
	write(OS, X3), write(OS, '=( '), write(OS, X4), nl(OS)}.

set(OS) --> [set], variable1(X), [=], chars1([],X1), !, {atomic_list_concat(X, '', X3), atomic_list_concat(X1, '', X4),
	write(OS, X3), write(OS, '='), write(OS, X4), nl(OS)}.

cd(OS) --> [cd], {write(OS, 'cd ')}, streamOrChars(OS), !.

mkdir(OS) --> [mkdir], {write(OS, 'mkdir ')}, streamOrChars(OS), !.

rm(OS) --> [rm], {write(OS, 'rm ')}, streamOrChars(OS), !.

rmdir(OS) --> [rmdir], {write(OS, 'rmdir ')}, streamOrChars(OS), !.

ps(OS) --> [ps], [-], {write(OS, 'ps -')}, check_alphabet(X), {write(OS, X)}, {write(OS, ' ')}, streamOrChars(OS), !.
ps(OS) --> [ps], {write(OS, 'ps ')}, streamOrChars(OS), !.

streamOrChars(OS) --> [>], {write(OS, '> ')}, streamOrChars(OS), !.
streamOrChars(OS) --> [X], [>], {write(OS, X), write(OS, ' > ')}, streamOrChars(OS), !.
streamOrChars(OS) --> [X], {write(OS, X)}, streamOrChars(OS), !.
streamOrChars(OS) --> [],  {nl(OS)}, !.


%%%%%%%%%%%%%%%%%%%%
%LOOPS
%%%%%%%%%%%%%%%%%%%%

while(OS) --> ['while'], {write(OS, 'while ( ')}, condition(OS), { write(OS, ' )'),
	nl(OS), write(OS, 'do'), nl(OS), read_new_line(OS, [end])}.
 
foreach(OS) --> ['foreach'], {write(OS, 'for ')}, check_alphabet(X), {write(OS, X), write(OS, ' in ')},
	['('], {write(OS,'$( ')}, chars2(OS), [')'], {write(OS,'); do'), nl(OS), read_new_line(OS, [end]) }.

finals(X, OS) --> ['end'], {write(OS, 'done'), nl(OS)}.
finals(X, OS) --> ['endif'], {write(OS, 'fi'), nl(OS)}.
finals([H|X], OS) --> ['else'], condStatement(OS,1).



%%%%%%%%%%%%%%%%%%%%
%Conditional statement
%%%%%%%%%%%%%%%%%%%%


condStatement(OS, B) --> ['if'],  ({B is 0} -> {write(OS, 'if ( ')}; {write(OS, 'elif (')}), condition(OS),
 ['then'], { write(OS, ' ) then '), nl(OS), read_new_line(OS, [else, endif])}.

%%%%%%%%%%%%%%%%%%%%
%UTILS
%%%%%%%%%%%%%%%%%%%%

variable(X) --> ['$'], check_alphabet(X2), {X = [X2]}.
variable(X) --> check_alphabet(X).
variable1(X) --> ['$'], check_alphabet(X2), {X = [X2]}.
variable1(X) --> check_alphabet(X1), {X = [X1]}.

condition(OS) -->  ['('], variable([H|_]), condition_sign(X11), { atomic_list_concat(X11, ' ', Z11) }, (check_number_alphabet(X4) ->
																				[')'], {atomic_list_concat(X4, '', Z4), write(OS, '$'), write(OS, H), write(OS, ' '), write(OS, Z11), write(OS, ' '), write(OS, Z4)};
																				variable(X1), [')'], {atomic_list_concat(X1, '', Z1), write(OS, Z1)}).

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

chars2(OS) --> [X1], ({cpk(X1)} -> chars2(OS); {write(OS, X1), write(OS, ' ')}, chars2(OS)).
chars2(OS) --> [].

cpk(X):- atom(X), atom_length(X,1), char_code(X,39) ;atom(X), atom_length(X,1), char_code(X,40) ;atom(X),
 atom_length(X,1), char_code(X,41) ;atom(X), atom_length(X,1), char_code(X,34). 

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
