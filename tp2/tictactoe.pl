	/*********************************
	DESCRIPTION DU JEU DU TIC-TAC-TOE
	*********************************/

	/*
	Une situation est decrite par un tableau 3x3
  	Les joueurs jouent � tour de role
	soit une croix (x) soit un rond (o)
	dans un emplacement libre.

	Pour mod�liser un emplacement initialement libre
	on utilise une variable libre.
	Pour mod�liser le placement d'un symbole il suffira
	alors d'instancier cette variable via le pr�dicat
	member/2 ou select/3, ou nth1/3 ...
			
	La situation initiale est une matrice 3x3 ne comportant
	que des variables libres

	DEFINIR ICI LA CLAUSE DEFINISSANT LA SITUATION INITIALE
	*/

situation_initiale([[A, B, C],[D, E, F],[x, x, I]]).

	/* Convention (arbitraire) : c'est x qui commence

	DEFINIR ICI LE JOUEUR QUI COMMENCE A JOUER EN PREMIER
	*/

joueur_initial(x).

	/* Definition de la relation adversaire/2
	DEFINIR ICI QUI EST ADVERSAIRE DE x ET QUI EST ADVERSAIRE DE o */

adversaire(x, o).
adversaire(o, x).

	/****************************************************
	 DEFINIR ICI � l'aide du pr�dicat ground/1 comment
	 reconnaitre une situation terminale due au fait qu'il
	 n'y a plus aucun emplacement libre pour le Joueur
	 (quel qu'il soit)
	 ****************************************************/

situation_terminale(_Joueur, Situation) :- 
	ground(Situation).

/**************************
 DEFINITION D'UN ALIGNEMENT
 **************************/

alignement(L, Matrix) :- ligne(    L,Matrix).
alignement(C, Matrix) :- colonne(  C,Matrix).
alignement(D, Matrix) :- diagonale(D,Matrix).

	/**********************************************
	 COMPLETER LES DEFINITIONS des diff�rents types
 	 d'alignements existant dans une matrice carree
	 **********************************************/
	
% L est une liste repr�sentant une ligne de la matrice M
ligne(L, M) :- 
	member(L, M).

% C est une liste repr�sentant une colonne de la matrice M

colonne(C,M) :- 
	col(N, M, C).

col(_,[],[]).
col(N,[L|M],[X|C]) :-
	nth1(N,L,X),
	col(N,M,C).

% D est une liste repr�sentant une diagonale de la matrice M 
% il y en a 2 sortes

diagonale(D,M) :- diago_desc(1,M,D).
diagonale(D,M) :- length(M,LEN), diago_mont(LEN,M,D).

diago_desc(_,[],[]).
diago_desc(N,[L1|M],[X|D]) :-
	nth1(N,L1,X),
	Nsuiv is N+1,
	diago_desc(Nsuiv,M,D).

% A FAIRE :
diago_mont(_,[],[]).
diago_mont(N,[L1|M],[X|D]) :-
	nth1(N,L1,X),
	Nsuiv is N-1,
	diago_mont(Nsuiv,M,D).


	/***********************************
	 DEFINITION D'UN ALIGNEMENT POSSIBLE
	 POUR UN JOUEUR DONNE
	 **********************************/

alignement_possible(J,Ali,M) :- alignement(Ali,M), possible(Ali,J).

possible([X|L], J) :- unifiable(X,J), possible(L,J).
possible([   ], _).

	/* Attention 
	il faut juste verifier le caractere unifiable
	de chaque emplacement de la liste, mais il ne
	faut pas realiser l'unification.
	*/

% A FAIRE 
unifiable(X,_) :- var(X).
unifiable(X,J) :- ground(X), X=J.
	
	/**********************************
	 DEFINITION D'UN ALIGNEMENT GAGNANT
	 OU PERDANT POUR UN JOUEUR DONNE J
	 **********************************/

	/*
	Un alignement gagnant pour J est un alignement
possible pour J qui n'a aucun element encore libre.
Un alignement perdant pour J est gagnant
pour son adversaire.
	*/

% A FAIRE

alignement_gagnant(Ali, J) :- foreach(member(X, Ali), (ground(X),X=J)).
alignement_perdant(Ali, J) :- adversaire(J, A), alignement_gagnant(Ali, A).


	/******************************
	DEFINITION D'UN ETAT SUCCESSEUR
	*******************************/

	/* 
	Il faut d�finir quelle op�ration subit
	une matrice M representant la situation courante
	lorsqu'un joueur J joue en coordonnees [L,C]
	*/	

% A FAIRE
successeur(J,Etat,[L,C]) :- 
	nth1(L, Etat, Ligne),
	nth1(C, Ligne, X),
	var(X), X = J.

	/**************************************
   	 EVALUATION HEURISTIQUE D'UNE SITUATION
  	 **************************************/

	/*
1/ l'heuristique est +infini si la situation J est gagnante pour J
2/ l'heuristique est -infini si la situation J est perdante pour J
3/ sinon, on fait la difference entre :
	   le nombre d'alignements possibles pour J
	moins
 	   le nombre d'alignements possibles pour l'adversaire de J
*/


heuristique(J,Situation,H) :-		% cas 1
   H = 10000,				% grand nombre approximant +infini
   alignement(Alig,Situation),
   alignement_gagnant(Alig,J), !.
	
heuristique(J,Situation,H) :-		% cas 2
   H = -10000,				% grand nombre approxCimant -infini
   alignement(Alig,Situation),
   alignement_perdant(Alig,J),!.	


% on ne vient ici que si les cut precedents n'ont pas fonctionne,
% c-a-d si Situation n'est ni perdante ni gagnante.
?- situation_initiale(I), alignement_possible(x, [x, x, x], I
% A FAIRE 					cas 3
heuristique(J,Situation,H) :- 
	findall(Ali, alignement_possible(J, Ali, Situation), Tab_J),
	length(Tab_J, Cnt_J),
	adversaire(J, A),
	findall(Ali, alignement_possible(A, Ali, Situation), Tab_A),
	length(Tab_A, Cnt_A),
	H is Cnt_J - Cnt_A.
	
	


