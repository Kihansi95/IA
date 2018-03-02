:-[avl].
:-[taquin].

main :-
	% etat init
	initial_state(S0),
	heuristique(S0, H0),
	G0 is 0, F0 is H0 + G0,

	% init Pf, Pu et Q
	empty(Pf0), insert([[F0, H0, G0], S0], Pf0, Pf1),
	empty(Pu0), insert([S0, [F0, H0, G0], nil, nil] , Pu0, Pu1),
	empty(Q0),

	% commencer la recherche
	aetoile(Pf1, Pu1, Q0).

affiche_solution(nil, _).
affiche_solution(S, Q) :-
	belongs([S, _, S_Pere, Action], Q),
	affiche_solution(S_Pere, Q),
	write(Action), write(" -> "), write(S), write(", ").

expand(G, U, S_List) :- 
	findall(S, (
			rule(R, 1, U, E), 			
			heuristique(E, Hs), Gs is G +1, Fs is Gs + Hs, 
			S = [[Fs, Hs, Gs], E, R]
			), S_List).

loop_successors([], _, Val_old, Val_new):-
	Val_new = Val_old.
% si S est connue dans Q alors oublier cet état 
loop_successors([[_, E, _]|Rest], U, [Pf, Pu, Q], AVLs_new) :-
	loop_successors(Rest, U, [Pf, Pu, Q], AVLs_new),
	belongs([E, _, _, _], Q).
% si S est connue dans Pu alors garder le terme associé à la meilleure évaluation
/*loop_successors([[[Fs, Hs, Gs], E, R]|Rest], U, [Pf, Pu, Q], [Pf_new, Pu_new, Q_new]) :-
	loop_successors([Rest], [Pf, Pu, Q], [Pf1, Pu1, Q1])
	belongs([E, [F, _, _], _, _], Pu),
	Fs < F,
	suppress([E, _, _, _], Pu, Pu1),
	insert([E, [Fs, Hs, Gs], U, R], Pu1, Pu_new).
	suppress([_, E], Pf, Pf1),
	insert([[Fs, Hs, Gs], E], Pf1, Pf_new). */
loop_successors([[Val_new, E, R]|Rest], U, [Pf, Pu, Q], [Pf_new, Pu_new, Q]) :-
	loop_successors(Rest, U, [Pf, Pu, Q], [Pf1, Pu1, Q]),
	belongs([E, Val, _, _], Pu1),
	Val_new @< Val,
	suppress([E, _, _, _], Pu1, Pu1),
	insert([E, Val_new, U, R], Pu2, Pu_new),
	suppress([_, E], Pf1, Pf2),
	insert([Val_new, E], Pf2, Pf_new).
% sinon S est une situation nouvelle
loop_successors([[Val_new, E, R]|Rest], [Pf, Pu, Q], [Pf_new, Pu_new, Q]) :-
	loop_successors(Rest, U,[Pf, Pu, Q], [Pf1, Pu1, Q]),
	insert([E, Val_new, U, R], Pu1, Pu_new),
	insert([Val_new, E], Pf1, Pf_new).


aetoile(_, Pu, _) :-
	empty(Pu),
	writeln("Pas de solution").
aetoile(Pf, Pu, Q) :-
	%belongs([[_, 0, _], U], Pf),
	belongs([U, Val, Pere, Action], Pu),
	final_state(F_S), U = F_S, !,		% ne pas chercher d'autre clause
	insert([U, Val, Pere, Action], Q, Q1),
	affiche_solution(U, Q1).
aetoile(Pf, Pu, Q) :-
	suppress_min([[F, H, G], U], Pf, Pf1),
	suppress([U, _, Pere, Action], Pu, Pu1),
	insert([U, _, Pere, Action], Q, Q1),
	expand(G, U, S_List),
	loop_successors(S_List, U, [Pf1, Pu1, Q1], [Pf2, Pu2, Q2]),
	aetoile(Pf2, Pu2, Q2).
		

