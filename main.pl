% Autor: Michal Majer, xmajer21
% Datum: 2023-04-27
% FLP projekt 2 - Babylonská věž

% Define a module
:- module(main, [main/0]).

%
% Input / output
%

% Reads stdin, returns list of lines
readLines(Lines) :-
    readLine(Line, LastChar),
    (LastChar == end_of_file, Lines = []; readLines(T), Lines = [Line|T]).

% Reads line from stdin, returns line and last char
readLine(Line, LastChar) :-
    get_char(Char),
    (isEOForEOL(Char) -> (Line = [], LastChar = Char); (readLine(L, C), Line = [Char|L], LastChar = C)).

% Check if char is end of line or end of file
isEOForEOL(Char) :- Char == end_of_file; (char_code(Char, Code), Code == 10).

% Convert token to numeric representation
% A1 -> [0, 0], B1 -> [0, 1], A2 -> [1, 0]
% A is first column, not row
getIndexFromToken(Token, R:C) :- atom_chars(Token, [Letter, Number]), char_code(Letter, L), char_code(Number, N),
    !, ( Letter == '*', Number == '*' ->  R is -1, C is -1; R is N - 49, C is L - 65).

getTokenFromIndex(-1: -1, **) :- !.
getTokenFromIndex(R:C, Token) :- Cchar is C + 65, Rchar is R + 49, atom_chars(Token, [Cchar, Rchar]).

convertTokensToIndexes(Tokens, Indexes) :- maplist(convertRowToIndexes, Tokens, Indexes).
convertRowToIndexes(Row, Indexes) :- maplist(getIndexFromToken, Row, Indexes).

printConfig(S) :- foreach(member(Row, S), printRow(Row)), nl.
printRow(Row) :- foreach(member(Token, Row), printToken(Token)), nl.
printToken(Token) :- getTokenFromIndex(Token, TokenString), write(TokenString), write(' ').

list_to_words([], []).
list_to_words([Word], [Word]).
list_to_words([Atom1, Atom2], [Word]) :-
    atom_concat(Atom1, Atom2, Word).
list_to_words([Atom1, Atom2, _ | Rest], [Word | Words]) :-
    atom_concat(Atom1, Atom2, Word),
    list_to_words(Rest, Words).

splitLines([], []).
splitLines([Line|Lines], [Words|SplitLines]) :-
    list_to_words(Line, Words),
    splitLines(Lines, SplitLines).

%
% Dimensions
%

:- dynamic width/1, height/1.

% Get the dimensions of the tower
% Count number of lines, get width of first line
getDimensions(Lines, W, H) :- length(Lines, H), nth0(0, Lines, Line), length(Line, W).

% Generate numbers from 1 to width-1
oneToWidth(O) :- width(W), Wless is W - 1, between(1, Wless, O).

% Generate numbers from 0 to height-1
zeroToHeight(O) :- height(H), Wheight is H - 1, between(0, Wheight, O).

%
% Moves
%

% Moves
% every row can be rotated
% token from above or below ** can be moved to ** position
move(S, D) :- oneToWidth(W), zeroToHeight(H),  rotate(S, H, W, D).
move(S, D) :- emptySpaceIndex(S, R:C), zeroToHeight(H), H \= R, shiftColumn(S, C, R, H, D).

% Shift column C: shift tokens Empty to End to Empty
shiftColumn(S, _, R1, R1, S) :- !.
shiftColumn(S, C, Empty, End, D) :- Empty < End, Emptyplus is Empty + 1, 
    swapColumn(S, C, Empty, Emptyplus, S1), shiftColumn(S1, C, Emptyplus, End, D).
shiftColumn(S, C, Empty, End, D) :- Empty > End, Emptyminus is Empty - 1, 
    swapColumn(S, C, Empty, Emptyminus, S1), shiftColumn(S1, C, Emptyminus, End, D).

% Replace element at index I with X
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% Swap two tokens in column C of row R1 and R2
swapColumn(S, C, R1, R2, D) :- nth0(R1, S, Row1), nth0(R2, S, Row2), nth0(C, Row1, T1), nth0(C, Row2, T2),
    replace(Row1, C, T2, Row1new), replace(Row2, C, T1, Row2new), replace(S, R1, Row1new, S1), replace(S1, R2, Row2new, D), !.

% Find empty space **
emptySpaceIndex(S, R:C) :- zeroToHeight(R), nth0(R, S, Row), emptySpaceIndexRow(Row, C).
emptySpaceIndexRow(Row, Col) :- nth0(Col, Row, R:C), R == -1, C == -1, !.

% Rotate a row R by W to the left
rotate(S, _, 0, S) :- !.
rotate(S, R, W, D) :- Wless is W - 1, rotate(S, R, Wless, S1), rotateByOne(S1, R, D).

% Rotate a row R by 1 to the left
rotateByOne(S, R, D) :- nth0(R, S, Row), rotateByOne(Row, DRow), replace(S, R, DRow, D), !.
rotateByOne([H|T], D) :- append(T, [H], D).

%
% Goal
%

:- dynamic final/1.

% Create final configuration (for comparing with current configuration)
getFinal(Final) :- height(H), Hless is H - 1,
    findall(Row, (between(0, Hless, Hi), createRow(Hi, Row)), Final).
createRow(Rowindex, Row) :- width(W), Wless is W - 1,
    findall(R:C, (between(0, Wless, Wi), createToken(Rowindex, Wi, R:C)), Row).
createToken(Rowindex, Colindex, R:C) :- height(H), Hless is H - 1,
    (Rowindex == Hless, Colindex == 0 -> R = -1, C = -1 ; R = Rowindex, C = Colindex).

isFinal(S) :- final(F), compareFinal(S, F).
compareFinal([], []).
compareFinal([R1|S1], [R2|S2]) :- compareFinalRow(R1, R2), compareFinal(S1, S2).
compareFinalRow([], []).
compareFinalRow([T1|Row1], [T2|Row2]) :- compareFinalToken(T1, T2), compareFinalRow(Row1, Row2).
compareFinalToken(R1:C1, R2:C2) :- R1 == R2, C1 == C2.

% Get all sequences of moves L from initial state S of length N, with moves M
getSequence(S, 1, [S]).
getSequence(S, N, [S|T]) :- N > 1, Nless is N - 1,
    move(S, H), getSequence(H, Nless, T).

solve(Init, Seq) :- getFinal(F), assert(final(F)), between(1, 40, N), getSequence(Init, N, Seq), append(_, [Last], Seq), isFinal(Last).

% Main predicate

main :-
    prompt(_, ''),
    readLines(InputLines),
    splitLines(InputLines, Tokens),
    getDimensions(Tokens, W, H),
    convertTokensToIndexes(Tokens, Indexes),
    assert(width(W)),
    assert(height(H)), !,
    solve(Indexes, L),
    foreach(member(M, L), printConfig(M)).
