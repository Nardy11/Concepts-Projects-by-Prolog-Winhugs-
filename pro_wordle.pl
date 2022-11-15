
% Game.
	main :- 
			build_kb,
			play,
			write('Note: to clear kb write ?- clear_kb.'),nl.
			
	%**************************************************************%
	
	
% Building kb phase.
build_kb :- 
			write('Welcome to Pro-Wordle!'),nl,
			write('----------------------'),nl,nl,
			words_and_categories,
			write('Done building the words database...'),nl,nl.

words_and_categories:- 
					write('Please enter a word and its category on separate lines:'),nl,
					read(Word),
					(
						Word == done,
						(
							word(_,_),nl;
							\+word(_,_),
							write('You should enter at least one word and its category.'),nl,
							words_and_categories
						);
						read(Category),
						nonvar(Word),
						nonvar(Category),
						assert(word(Word,Category)),
						words_and_categories;
						(
							write('Please do not enter variables! '), nl,
							words_and_categories
							)
						).

	%**************************************************************%
	
% Playing phase.
	play :- 
			write('The available categories are: '),
			categories(List),
			write(List),nl,
			choose_a_category(Category,List),
			choose_a_length(Length,Category),
			write('Game started. You have '),
			N is Length + 1,
			write(N),
			write(' guesses.'),nl,nl,
			pick_random_word(Word,Length,Category),
			guess(Word,N,Length).
			
	%**************************************************************%
	
% Clear kb.
	clear_kb :- 
			word(_,_),
			retractall(word(_,_)),	
			write('Done! kb is empty.'), ! ;
			write('kb is already empty!'),
			retract(word(_,_)).
			
	%**************************************************************%
	
% Helper methods.

:- dynamic(word/2).

choose_a_category(Category,List) :- 
					write('choose a category:'),nl,
					read(X),
					(
						member(X,List),
						Category = X;
						write('This category does not exist.'),nl,
						choose_a_category(Category,List)
					).
					
choose_a_length(Length,Category) :- 
					write('Choose a length:'),nl,
					read(X),
					(
						integer(X),
						pick_word(_,X,Category),
						Length = X,nl ;
						integer(X),
						write('There are no words of this length.'),nl,
						choose_a_length(Length,Category)
					);(
						write('The length you entered is not a number.'),nl,
						choose_a_length(Length, Category)
					).
guess(_,0,_) :- write('You lost!'),nl.	
guess(Word,N,Length) :- 
				write('Enter a word composed of '),
				write(Length),
				write(' letters:'),nl,
				read(X),
				nonvar(X),
				(
					X = Word,
					write('You Won!'),nl
					;
					string_length(X,L),
					L \= Length,
					write('Word is not composed of '),
					write(Length),
					write('  letters. Try again.'),nl,
					write('Remaining Guesses are '),
					write(N),nl,nl,
					guess(Word,N,Length)
					;
					\+word(X,_),
					write('Word is not in the Knowledge Base! Try again.'),nl,
					write('Remaining Guesses are '),
					write(N),nl,nl,
					guess(Word,N,Length)
					;
					N1 is N - 1,
					(		
						N1 \= 0,
						string_chars(X,L1),
						string_chars(Word,L2),
						correct_letters(L1,L2,CL),
						correct_positions(L1,L2,CP),
						write('Correct letters are: '),
						write(CL),nl,
						write('Correct letters in correct positions are '),
						write(CP),nl,
						write('Remaining Guesses are '),
						write(N1),nl,nl,
						guess(Word,N1,Length);
						guess(Word,N1,Length)
					)
				);
				(
				  write('Please do not enter variables! '), nl,
				  		guess(Word, N, Length) 
					).
				
				

is_category(C) :- word(_,C).

categories(L) :- setof(C,is_category(C),L).

available_length(L) :- 
					word(X,_),
					string_length(X,L).
					
pick_word(W,L,C) :-
					word(W,C),
					string_length(W,L).
					
pick_random_word(W,L,C) :- 
					setof(W,pick_word(W,L,C),R),
					random_member(W,R).


correct_letters(L1,L2,CL) :- 
					intersection(L1,L2,L),
					(
						L = [],
						CL = [];
						setof(Char, member(Char,L),CL)
					).

correct_positions([],[],[]).					
correct_positions([H|T1],[H|T2],[H|T]) :- correct_positions(T1,T2,T).
correct_positions([_|T1],[_|T2],PL) :- correct_positions(T1,T2,PL).
