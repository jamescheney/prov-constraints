% James Cheney, February 6, 2013
% Some  Prolog code for PROV-CONSTRAINTS

:- consult(prov_xml).




remove(X,[Y|J],J) :- X == Y,!.
remove(X,[Y|J],[Y|K]) :- X \== Y, remove(X,J,K).
	
multimember(A,B,[A|L]) :- member(B,L).
multimember(A,B,[_|L]) :- multimember(A,B,L).

ensure(A,I,J) :- \+((member(B,I), A==B)),  J = [A|I].

% helper for influence 15
inferInfluence(Id,X,Y,Attrs1,I,J) :-
	member(wasInfluencedBy(Id,X,Y,Attrs2),I)
	-> (subset(Attrs1,Attrs2)
	   -> fail
	   ; (append(Attrs1,Attrs2,Attrs),
	      remove(wasInfluencedBy(Id,X,Y,Attrs2),I,J1),
	      J = [wasInfluencedBy(Id,X,Y,Attrs)|J1]))
	; J = [wasInfluencedBy(Id,X,Y,Attrs1)|I].



% inference 5
infer(I,J) :- member(wasInformedBy(_Id,A2,A1,_Attrs),I),
	\+((member(wasGeneratedBy(Gen,E,A1,_,_),I),
	    member(used(Use,A2,E,_,_),I))),
	J = [wasGeneratedBy(Gen,E,A1,_T1,[]),
	     used(Use,A2,E,_T2,[])|I].

% inference 6
infer(I,J) :- member(wasGeneratedBy(_Gen,E1,A1,_T1,_Attrs1),I),
	      member(used(_Use,A2,E2,_T2,_Attrs2),I),
              E1 == E2,
	   \+(member(wasInformedBy(_,A2,A1,_),I)),
	   J = [wasInformedBy(_Id,A2,A1,[])|I].

% inference 7a
infer(I,J) :- member(entity(E,_Attrs),I),
              \+(member(wasGeneratedBy(_,E,_,_,_),I)),
              J = [wasGeneratedBy(_,E,_,_,[])|I].
% inference 7b
infer(I,J) :- member(entity(E,_Attrs),I),
              \+(member(wasInvalidatedBy(_,E,_,_,_),I)),
              J = [wasInvalidatedBy(_,E,_,_,[])|I].

% inference 8a
infer(I,J) :- member(activity(A,T1,_T2,_Attrs),I),
              \+(member(wasStartedBy(_,A,_,_,T1,_),I)),
              J = [wasStartedBy(_,A,_,_,T1,_)|I].
% inference 8b
infer(I,J) :- member(activity(A,_T1,T2,_Attrs),I),
              \+(member(wasEndedBy(_,A,_,_,T2,_),I)),
              J = [wasEndedBy(_,A,_,_,T2,[])|I].

% inference 9
infer(I,J) :- member(wasStartedBy(_ID,_A,E1,A1,_T,_Attrs),I),
              \+(member(wasGeneratedBy(_,E1,A1,_,_),I)),
              J = [wasGeneratedBy(_,E1,A1,_,[])|I].

% inference 10
infer(I,J) :- member(wasEndedBy(_ID,_A,E1,A1,_T,_Attrs),I),
              \+(member(wasGeneratedBy(_,E1,A1,_,_),I)),
              J = [wasGeneratedBy(_,E1,A1,_,[])|I].

% inference 11a
infer(I,J) :- member(wasDerivedFrom(_ID,_E2,E1,A,Gen2,Use1,_Attrs),I),
	      A \== null, Gen2 \== null, Use1 \== null,
              \+(member(used(Use1,A,E1,_,_),I)),
              J = [used(Use1,A,E1,_,[])|I].
% inference 11b
infer(I,J) :- member(wasDerivedFrom(_ID,E2,_E1,A,Gen2,Use1,_Attrs),I),
	      A \== null, Gen2 \== null, Use1 \== null,
              \+(member(wasGeneratedBy(Gen2,E2,A,_,_),I)),
              J = [wasGeneratedBy(Gen2,E2,A,_,[])|I].

% inference 12
infer(I,J) :- member(wasDerivedFrom(_ID,E2,E1,_A,_G,_U,Attrs),I),
              member(Prov:'type'='prov:Revision', Attrs),
              ns(prov,Prov),
              \+(member(alternateOf(E2,E1),I)),
              J = [alternateOf(E2,E1)|I].

% inference 13
infer(I,J) :- member(wasAttributedTo(_ID,E,Ag,_Attrs),I),
              \+((member(wasGeneratedBy(_,E,A,_,_),I),
		 member(wasAssociatedWith(_,A,Ag,_,_),I))),
              J = [wasGeneratedBy(_,E,A,_,[]),
		   wasAssociatedWith(_,A,Ag,_,_)|I].

% inference 14a
infer(I,J) :- member(actedOnBehalfOf(_ID,Ag1,_Ag2,A,_Attrs),I),
              \+(member(wasAssociatedWith(_,A,Ag1,_,_),I)),
              J = [wasAssociatedWith(_,A,Ag1,_,[])|I].

% inference 14b
infer(I,J) :- member(actedOnBehalfOf(_ID,_Ag1,Ag2,A,_Attrs),I),
              \+(member(wasAssociatedWith(_,A,Ag2,_,_),I)),
              J = [wasAssociatedWith(_,A,Ag2,_,[])|I].

% Inference 15: tricky because of attribute inheritance
% Add if no previous influence 
% Should merge if there is already an influence, but this is tricky to 
% handle without diverging

% inference 15.1
infer(I,J) :- member(wasGeneratedBy(Id,E,A,_T,Attrs),I),
              inferInfluence(Id,E,A,Attrs,I,J).

% inference 15.2
infer(I,J) :- member(used(Id,A,E,_T,Attrs),I),
              inferInfluence(Id,A,E,Attrs,I,J).

% inference 15.3
infer(I,J) :- member(wasInformedBy(Id,Y,X,_T,Attrs),I),
              inferInfluence(Id,Y,X,Attrs,I,J).

% inference 15.4
infer(I,J) :- member(wasStartedBy(Id,Y,X,_,_T,Attrs),I),
              inferInfluence(Id,Y,X,Attrs,I,J).


% inference 15.5
infer(I,J) :- member(wasEndedBy(Id,Y,X,_,_T,Attrs),I),
              inferInfluence(Id,Y,X,Attrs,I,J).


% inference 15.6
infer(I,J) :- member(wasInvalidatedBy(Id,E,A,_T,Attrs),I),
              inferInfluence(Id,E,A,Attrs,I,J).


% inference 15.7
infer(I,J) :- member(wasDerivedFrom(Id,Y,X,_A,_G,_U,Attrs),I),
              inferInfluence(Id,Y,X,Attrs,I,J).


% inference 15.8
infer(I,J) :- member(wasAttributedTo(Id,Y,X,Attrs),I),
              inferInfluence(Id,Y,X,Attrs,I,J).


% inference 15.9
infer(I,J) :- member(wasAssociatedWith(Id,Y,X,_Pl,Attrs),I),
              inferInfluence(Id,Y,X,Attrs,I,J).


% inference 15.9
infer(I,J) :- member(actedOnBehalfOf(Id,Y,X,_A,Attrs),I),
              inferInfluence(Id,Y,X,Attrs,I,J).


% inference 16

infer(I,J) :- member(entity(E,_Attrs),I),
              \+(member(alternateOf(E,E),I)),
              J = [alternateOf(E,E)|I].

% inference 17
% careful about unification
infer(I,J) :- member(alternateOf(E1,E21),I),
              member(alternateOf(E22,E3),I),
              E21==E22,
              \+(member(alternateOf(E1,E3),I)),
              J = [alternateOf(E1,E3)|I].

% inference 18
infer(I,J) :- member(alternateOf(E1,E2),I),
              \+(member(alternateOf(E2,E1),I)),
              J = [alternateOf(E2,E1)|I].

% inference 19
% careful about unification
infer(I,J) :- member(specializationOf(E1,E21),I),
              member(specializationOf(E22,E3),I),
              E21==E22,
              \+(member(specializationOf(E1,E3),I)),
              J = [specializationOf(E1,E3)|I].

% inference 20
infer(I,J) :- member(specializationOf(E1,E2),I),
              \+(member(alternateOf(E1,E2),I)),
              J = [alternateOf(E1,E2)|I].

% inference 21
infer(I,J) :- member(entity(E,Attrs),I),
              member(specializationOf(E2,E1),I),
              E1 == E,
              \+(member(entity(E2,Attrs),I)),
              J = [entity(E2,Attrs)|I].



% Constraint 22.1: key constraint: entity
infer(I,J) :- multimember(entity(ID1,Attrs1),
			  entity(ID2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	remove(entity(ID1,Attrs1),I,J1), % and merge
	remove(entity(ID2,Attrs2),J1,J2),
	append(Attrs1,Attrs2,Attrs),
	J = [entity(ID1,Attrs)|J2].

% Constraint 22.2: key constraint: activity
infer(I,J) :- multimember(activity(ID1,ST1,ET1,Attrs1),
			  activity(ID2,ST2,ET2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	 % And non-keys are unifiable, unify them
	((ST1=ST2, ET1=ET2)
	-> (
	  remove(activity(ID1,ST1,ET1,Attrs1),I,J1), % and merge
	  remove(activity(ID2,ST2,ET2,Attrs2),J1,J2),
	  append(Attrs1,Attrs2,Attrs),
	  J = [activity(ID1,ST1,ET1,Attrs)|J2])
	; J = invalid(I)).


% Constraint 22.3: key constraint: agent
infer(I,J) :- multimember(agent(ID1,Attrs1),
			  agent(ID2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	remove(agent(ID1,Attrs1),I,J1), % and merge
	remove(agent(ID2,Attrs2),J1,J2),
	append(Attrs1,Attrs2,Attrs),
	J = [agent(ID1,Attrs)|J2].

% Constraint 23.1: 
infer(I,J) :- multimember(wasGeneratedBy(ID1,E1,A1,T1,Attrs1),
			  wasGeneratedBy(ID2,E2,A2,T2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	 % And non-keys are unifiable, unify them
	((A1 = A2, E1 = E2, T1 = T2)
	-> (
	  remove(wasGeneratedBy(ID1,E1,A1,T1,Attrs1),I,J1), % and merge
	  remove(wasGeneratedBy(ID2,E2,A2,T2,Attrs2),J1,J2),
	  append(Attrs1,Attrs2,Attrs),
	  J = [wasGeneratedBy(ID1,E1,A1,T1,Attrs)|J2])
	; J = invalid(I)).

% Constraint 23.2: 
infer(I,J) :- multimember(used(ID1,A1,E1,T1,Attrs1),
			  used(ID2,A2,E2,T2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	 % And non-keys are unifiable, unify them
	((A1 = A2, E1 = E2, T1 = T2)
	-> (
	  remove(used(ID1,A1,E1,T1,Attrs1),I,J1), % and merge
	  remove(used(ID2,A2,E2,T2,Attrs2),J1,J2),
	  append(Attrs1,Attrs2,Attrs),
	  J = [used(ID1,A1,E1,T1,Attrs)|J2])
	; J = invalid(I)).

% Constraint 23.3: 
infer(I,J) :- multimember(wasInformedBy(ID1,A12,A11,Attrs1),
			  wasInformedBy(ID2,A22,A21,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	 % And non-keys are unifiable, unify them
	((A12 = A22, A11=A21)
	-> (
	  remove(wasInformedBy(ID1,A12,A11,Attrs1),I,J1), % and merge
	  remove(wasInformedBy(ID2,A22,A21,Attrs2),J1,J2),
	  append(Attrs1,Attrs2,Attrs),
	  J = [wasInformedBy(ID1,A12,A11,Attrs)|J2])
	; J = invalid(I)).

% Constraint 23.4: 
infer(I,J) :- multimember(wasStartedBy(ID1,A12,E1,A11,T1,Attrs1),
			  wasStartedBy(ID2,A22,E2,A21,T2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	 % And non-keys are unifiable, unify them
	((A12 = A22, A11=A21,E1=E2,T1=T2)
	-> (
	  remove(wasStartedBy(ID1,A12,E1,A11,T1,Attrs1),I,J1), % and merge
	  remove(wasStartedBy(ID2,A22,E2,A21,T2,Attrs2),J1,J2),
	  append(Attrs1,Attrs2,Attrs),
	  J = [wasStartedBy(ID1,A12,E1,A11,T1,Attrs)|J2])
	; J = invalid(I)).

% Constraint 23.5: 
infer(I,J) :- multimember(wasEndedBy(ID1,A12,E1,A11,T1,Attrs1),
			  wasEndedBy(ID2,A22,E2,A21,T2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	 % And non-keys are unifiable, unify them
	((A12 = A22, A11=A21,E1=E2,T1=T2)
	-> (
	  remove(wasEndedBy(ID1,A12,E1,A11,T1,Attrs1),I,J1), % and merge
	  remove(wasEndedBy(ID2,A22,E2,A21,T2,Attrs2),J1,J2),
	  append(Attrs1,Attrs2,Attrs),
	  J = [wasEndedBy(ID1,A12,E1,A11,T1,Attrs)|J2])
	; J = invalid(I)).

% Constraint 23.6: 
infer(I,J) :- multimember(wasInvalidatedBy(ID1,E1,A1,T1,Attrs1),
			  wasInvalidatedBy(ID2,E2,A2,T2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	 % And non-keys are unifiable, unify them
	((A1 = A2, E1 = E2, T1 = T2)
	-> (
	  remove(wasInvalidatedBy(ID1,E1,A1,T1,Attrs1),I,J1), % and merge
	  remove(wasInvalidatedBy(ID2,E2,A2,T2,Attrs2),J1,J2),
	  append(Attrs1,Attrs2,Attrs),
	  J = [wasInvalidatedBy(ID1,E1,A1,T1,Attrs)|J2])
	; J = invalid(I)).

% Constraint 23.7: 
infer(I,J) :- multimember(wasDerivedFrom(ID1,E12,E11,A1,G1,U1,Attrs1),
			  wasDerivedFrom(ID2,E22,E21,A2,G2,U2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	 % And non-keys are unifiable, unify them
	((E12=E22,E11=E21,A1 = A2, G1=G2, U1=U2)
	-> (
	  remove(wasDerivedFrom(ID1,E12,E11,A1,G1,U1,Attrs1),I,J1), % and merge
	  remove(wasDerivedFrom(ID2,E22,E21,A2,G2,U2,Attrs2),J1,J2),
	  append(Attrs1,Attrs2,Attrs),
	  J = [wasDerivedFrom(ID1,E12,E11,A1,G1,U1,Attrs)|J2])
	; J = invalid(I)).

% Constraint 23.8:
infer(I,J) :- multimember(wasAttributedTo(ID1,E1,Ag1,Attrs1),
			  wasAttributedTo(ID2,E2,Ag2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	 % And non-keys are unifiable, unify them
	((E1=E2,Ag1=Ag2)
	-> (
	  remove(wasAttributedTo(ID1,E1,Ag1,Attrs1),I,J1), % and merge
	  remove(wasAttributedTo(ID2,E2,Ag2,Attrs2),J1,J2),
	  append(Attrs1,Attrs2,Attrs),
	  J = [wasAttributedTo(ID1,E1,Ag1,Attrs)|J2])
	; J = invalid(I)).

% Constraint 23.9:
infer(I,J) :- multimember(wasAssociatedWith(ID1,A1,Ag1,Pl1,Attrs1),
			  wasAssociatedWith(ID2,A2,Ag2,Pl2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	 % And non-keys are unifiable, unify them
	((A1=A2,Ag1=Ag2,Pl1=Pl2)
	-> (
	  remove(wasAssociatedWith(ID1,A1,Ag1,Pl1,Attrs1),I,J1), % and merge
	  remove(wasAssociatedWith(ID2,A2,Ag2,Pl2,Attrs2),J1,J2),
	  append(Attrs1,Attrs2,Attrs),
	  J = [wasAssociatedWith(ID1,A1,Ag1,Pl1,Attrs)|J2])
	; J = invalid(I)).

% Constraint 23.10:
infer(I,J) :- multimember(actedOnBehalfOf(ID1,Ag12,Ag11,A1,Attrs1),
			  actedOnBehalfOf(ID2,Ag22,Ag21,A2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	 % And non-keys are unifiable, unify them
	((Ag12=Ag22,Ag11=Ag21,A1=A2)
	-> (
	  remove(actedOnBehalfOf(ID1,Ag12,Ag11,A1,Attrs1),I,J1), % and merge
	  remove(actedOnBehalfOf(ID2,Ag22,Ag21,A2,Attrs2),J1,J2),
	  append(Attrs1,Attrs2,Attrs),
	  J = [actedOnBehalfOf(ID1,Ag12,Ag11,A1,Attrs)|J2])
	; J = invalid(I)).

% Constraint 23.11:
infer(I,J) :- multimember(wasInfluencedBy(ID1,Y1,X1,Attrs1),
			  wasInfluencedBy(ID2,Y2,X2,Attrs2),I),
	ID1 == ID2, % If keys are equal terms
	 % And non-keys are unifiable, unify them
	((X1=X2,Y1=Y2)
	-> (
	  remove(wasInfluencedBy(ID1,Y1,X1,Attrs1),I,J1), % and merge
	  remove(wasInfluencedBy(ID2,Y2,X2,Attrs2),J1,J2),
	  append(Attrs1,Attrs2,Attrs),
	  J = [wasInfluencedBy(ID1,Y1,X1,Attrs)|J2])
	; J = invalid(I)).




% Constraint 24: unique-generation
infer(I,J) :- multimember(wasGeneratedBy(Gen1,E1,A1,_T1,_Attrs1),
			  wasGeneratedBy(Gen2,E2,A2,_T2,_Attrs2),I),
	E1 == E2, % If entity and activity are equal terms
	A1 == A2,
	((Gen1 = Gen2)
	-> (J = I) % And the ids unify, then done.  Key constraint will do the rest.
	; J = invalid(I)).	% Otherwise invalid.

% Constraint 25: unique-invalidation
infer(I,J) :- multimember(wasInvalidatedBy(Inv1,E1,A1,_T1,_Attrs1),
			  wasInvalidatedBy(Inv2,E2,A2,_T2,_Attrs2),I),
	E1 == E2, % If entity and activity are equal terms
	A1 == A2,
	((Inv1 = Inv2)
	-> (J = I) % And the ids unify, then done.  Key constraint will do the rest.
	; J = invalid(I)).	% Otherwise invalid.


% Constraint 26: unique-wasStartedBy
infer(I,J) :- multimember(wasStartedBy(ID1,A1,_E1,A11,_T1,_Attrs1),
			  wasStartedBy(ID2,A2,_E2,A12,_T2,_Attrs2),I),
	A1 == A2, % If entity and activity are equal terms
	A11 == A12,
	((ID1 = ID2)
	-> (J = I) % And the ids unify, then done.  Key constraint will do the rest.
	; J = invalid(I)).	% Otherwise invalid.

% Constraint 27: unique-wasEndedBy
infer(I,J) :- multimember(wasEndedBy(ID1,A1,_E1,A11,_T1,_Attrs1),
			  wasEndedBy(ID2,A2,_E2,A12,_T2,_Attrs2),I),
	A1 == A2, % If entity and activity are equal terms
	A11 == A12,
	((ID1 = ID2)
	-> (J = I) % And the ids unify, then done.  Key constraint will do the rest.
	; J = invalid(I)).	% Otherwise invalid.

% Constraint 28: unique-startTime
infer(I,J) :- member(activity(A,T1,_T2,_Attrs),I),
              member(wasStartedBy(_Start,A2,_E,_A1,T,_Attrs2),I),
              A==A2,
              T1 \== T,
              ((T1=T) 
	       -> (J = I)
	       ; J = invalid(I)).

% Constraint 29: unique-endTime
infer(I,J) :- member(activity(A,_T1,T2,_Attrs),I),
              member(wasEndedBy(_End,A2,_E,_A1,T,_Attrs2),I),
              A==A2,
              T2 \== T,
              ((T2=T) 
	       -> (J = I)
	       ; J = invalid(I)).





%% From this point onward we can be more relaxed about uification because
%% all the free variables have been frozen

constrain(P,O1,O2) :- \+((member(P,O1))),
	O2 = [P|O1].

% Constraint 30
ordering_step(I,O1,O2) :-
	member(wasStartedBy(Start,A,_E1,_A1,_T1,_Attrs1),I),
	member(wasEndedBy(End,A,_E2,_A2,_T2,_Attrs2),I),
	constrain(precedes(Start,End),O1,O2).

%Constraint 31
ordering_step(I,O1,O2) :-
	member(wasStartedBy(Start1,A,_E1,_A1,_T1,_Attrs1),I),
	member(wasStartedBy(Start2,A,_E2,_A2,_T2,_Attrs2),I),
	constrain(precedes(Start1,Start2),O1,O2).

%Constraint 32
ordering_step(I,O1,O2) :-
	member(wasEndedBy(End1,A,_E1,_A1,_T1,_Attrs1),I),
	member(wasEndedBy(End2,A,_E2,_A2,_T2,_Attrs2),I),
	constrain(precedes(End1,End2),O1,O2).

% Constraint 33.1
ordering_step(I,O1,O2) :-
	member(wasStartedBy(Start,A,_E1,_A1,_T1,_Attrs1),I),
	member(used(Use,A,_E2,_T2,_Attrs2),I),
	constrain(precedes(Start,Use),O1,O2).

% Constraint 33.2
ordering_step(I,O1,O2) :-
	member(used(Use,A,_E1,_T1,_Attrs1),I),
	member(wasEndedBy(End,A,_E2,_A2,_T2,_Attrs2),I),
	constrain(precedes(Use,End),O1,O2).

% Constraint 34.1
ordering_step(I,O1,O2) :-
	member(wasStartedBy(Start,A,_E1,_A1,_T1,_Attrs1),I),
	member(wasGeneratedBy(Gen,_E2,A,_T2,_Attrs2),I),
	constrain(precedes(Start,Gen),O1,O2).

% Constraint 34.2
ordering_step(I,O1,O2) :-
	member(wasGeneratedBy(Use,_E1,A,_T1,_Attrs1),I),
	member(wasEndedBy(End,A,_E2,_A2,_T2,_Attrs2),I),
	constrain(precedes(Use,End),O1,O2).

% Constraint 35
ordering_step(I,O1,O2) :-
	member(wasInformedBy(_Id,A2,A1,_Attrs),I),
	member(wasStartedBy(Start,A1,_E1,_A11,_T1,_Attrs1),I),
	member(wasEndedBy(End,A2,_E2,_A22,_T2,_Attrs2),I),
	constrain(precedes(Start,End),O1,O2).

% Constraint 36
ordering_step(I,O1,O2) :-
	member(wasGeneratedBy(Gen,E,_A1,_T1,_Attrs1),I),
	member(wasInvalidatedBy(Inv,E,_A2,_T2,_Attrs2),I),
	constrain(precedes(Gen,Inv),O1,O2).

% Constraint 37
ordering_step(I,O1,O2) :-
	member(wasGeneratedBy(Gen,E,_A1,_T1,_Attrs1),I),
	member(used(Use,_A2,E,_T2,_Attrs2),I),
	constrain(precedes(Gen,Use),O1,O2).

% Constraint 38
ordering_step(I,O1,O2) :-
	member(used(Use,_A1,E,_T1,_Attrs1),I),
	member(wasInvalidatedBy(Inv,E,_A2,_T2,_Attrs2),I),
	constrain(precedes(Use,Inv),O1,O2).

% Constraint 39
ordering_step(I,O1,O2) :-
	member(wasGeneratedBy(Gen1, E,_A1,_T1,_Attrs1),I),  
	member(wasGeneratedBy(Gen2, E,_A2,_T2,_Attrs2),I),
	constrain(precedes(Gen1,Gen2),O1,O2).

%Constraint 40
ordering_step(I,O1,O2) :-
	member(wasInvalidatedBy(Inv1, E,_A1,_T1,_Attrs1),I),  
	member(wasInvalidatedBy(Inv2, E,_A2,_T2,_Attrs2),I),
	constrain(precedes(Inv1,Inv2),O1,O2).


% Constraint 41
ordering_step(I,O1,O2) :-
	member(wasDerivedFrom(_ID, _E2,_E1,A,Gen,Use,_Attrs),I),
	A \== null, Gen \== null, Use \== null,
	constrain(precedes(Use,Gen),O1,O2).

% Constraint 42
ordering_step(I,O1,O2) :-
	member(wasDerivedFrom(_ID, E2,E1,_A,_Gen,_Use,_Attrs),I),
	member(wasGeneratedBy(Gen1, E1,_A1,_T1,_Attrs1),I),  
	member(wasGeneratedBy(Gen2, E2,_A2,_T2,_Attrs2),I),  
	constrain(strictlyPrecedes(Gen1,Gen2),O1,O2).

%Constraint 43.1
ordering_step(I,O1,O2) :-
	member(wasGeneratedBy(Gen, E,_A1,_T1,_Attrs1),I),  
	member(wasStartedBy(Start, _A,E,_A2,_T2,_Attrs2),I),
	constrain(precedes(Gen,Start),O1,O2).

%Constraint 43.2
ordering_step(I,O1,O2) :-
	member(wasStartedBy(Start, _A,E,_A1,_T1,_Attrs1),I),  
	member(wasInvalidatedBy(Inv, E,_A2,_T2,_Attrs2),I),
	constrain(precedes(Start,Inv),O1,O2).

%Constraint 44.1
ordering_step(I,O1,O2) :-
	member(wasGeneratedBy(Gen, E,_A1,_T1,_Attrs1),I),  
	member(wasEndedBy(End, _A,E,_A2,_T2,_Attrs2),I),
	constrain(precedes(Gen,End),O1,O2).

%Constraint 44.2
ordering_step(I,O1,O2) :-
	member(wasEndedBy(End, _A,E,_A1,_T1,_Attrs1),I),  
	member(wasInvalidatedBy(Inv, E,_A2,_T2,_Attrs2),I),
	constrain(precedes(End,Inv),O1,O2).

%Constraint 45
ordering_step(I,O1,O2) :-
	member(specializationOf(E2,E1),I),  
	member(wasGeneratedBy(Gen1, E1,_A1,_T1,_Attrs1),I),  
	member(wasGeneratedBy(Gen2, E2,_A2,_T2,_Attrs2),I),  
	constrain(precedes(Gen1,Gen2),O1,O2).

%Constraint 46
ordering_step(I,O1,O2) :-
	member(specializationOf(E1,E2),I),  
	member(wasInvalidatedBy(Inv1, E1,_A1,_T1,_Attrs1),I),  
	member(wasInvalidatedBy(Inv2, E2,_A2,_T2,_Attrs2),I),  
	constrain(precedes(Inv1,Inv2),O1,O2).

% Constraint 47.1
ordering_step(I,O1,O2) :-
	member(wasAssociatedWith(_Assoc,A,Ag,_Pl,_Attrs),I),  
	member(wasStartedBy(Start1, A,_E1,_A1,_T1,_Attrs1),I),  
	member(wasInvalidatedBy(Inv2, Ag,_A2,_T2,_Attrs2),I),  
	constrain(precedes(Start1,Inv2),O1,O2).

% Constraint 47.2
ordering_step(I,O1,O2) :-
	member(wasAssociatedWith(_Assoc,_A,Ag,_Pl,_Attrs),I),  
	member(wasGeneratedBy(Gen1, Ag,_A1,_T1,_Attrs1),I),  
	member(wasEndedBy(End2, Ag,_E2,_A2,_T2,_Attrs2),I),  
	constrain(precedes(Gen1,End2),O1,O2).

% Constraint 47.3
ordering_step(I,O1,O2) :-
	member(wasAssociatedWith(_Assoc,A,Ag,_Pl,_Attrs),I),  
	member(wasStartedBy(Start1, A,_E1,_A1,_T1,_Attrs1),I),  
	member(wasEndedBy(End2, Ag,_E2,_A2,_T2,_Attrs2),I),  
	constrain(precedes(Start1,End2),O1,O2).

% Constraint 47.4
ordering_step(I,O1,O2) :-
	member(wasAssociatedWith(_Assoc,A,Ag,_Pl,_Attrs),I),  
	member(wasStartedBy(Start1, Ag,_E1,_A1,_T1,_Attrs1),I),  
	member(wasEndedBy(End2, A,_E2,_A2,_T2,_Attrs2),I),  
	constrain(precedes(Start1,End2),O1,O2).

% Constraint 48.1
ordering_step(I,O1,O2) :-
	member(wasAttributedTo(_Att,E,Ag,_Attrs),I),  
	member(wasGeneratedBy(Gen1, Ag,_A1,_T1,_Attrs1),I),  
	member(wasGeneratedBy(Gen2, E,_A2,_T2,_Attrs2),I),  
	constrain(precedes(Gen1,Gen2),O1,O2).

% Constraint 48.2
ordering_step(I,O1,O2) :-
	member(wasAttributedTo(_Att,E,Ag,_Attrs),I),  
	member(wasStartedBy(Start1, Ag,_E1,_A1,_T1,_Attrs1),I),  
	member(wasGeneratedBy(Gen2, E,_A2,_T2,_Attrs2),I),  
	constrain(precedes(Start1,Gen2),O1,O2).

% Constraint 49.1
ordering_step(I,O1,O2) :-
	member(actedOnBehalfOf(_Del,Ag2,Ag1,_A,_Attrs),I),  
	member(wasGeneratedBy(Gen1, Ag1,_A1,_T1,_Attrs1),I),  
	member(wasInvalidatedBy(Inv2, Ag2,_A2,_T2,_Attrs2),I),  
	constrain(precedes(Gen1,Inv2),O1,O2).

% Constraint 49.2
ordering_step(I,O1,O2) :-
	member(actedOnBehalfOf(_Del,Ag2,Ag1,_A,_Attrs),I),  
	member(wasStartedBy(Start1, Ag1,_E1,_A1,_T1,_Attrs1),I),  
	member(wasEndedBy(End2, Ag2,_E2,_A2,_T2,_Attrs2),I),  
	constrain(precedes(Start1,End2),O1,O2).


% Find all existentially quantified vars and freeze them to fresh constants

fresh(A,I,J) :- J is I+1, atom_concat('fresh:id',I,A).

freeze([],[]) --> [].
freeze([T|L],[U|M]) --> freeze_term(T,U), freeze(L,M).

freeze_term(T,U) --> {T =.. [Name|Args]},
                     freeze_list(Args,Args2),
		     {U =.. [Name|Args2]}.

freeze_list([],[]) --> [].
freeze_list([Attrs],[Attrs2]) --> {nonvar(Attrs),(Attrs=[]; Attrs=[_|_])},
	freeze_attrs(Attrs,Attrs2).
freeze_list([A|Xs],[B|Ys]) --> freeze_arg(A,B), freeze_list(Xs,Ys).

freeze_arg(A,B) --> {atom(A),A=B}.
freeze_arg(X,Y) --> {var(X)}, fresh(Y), {Y=X}.

freeze_attrs([],[]) --> [].
freeze_attrs([A=X|AXs],[A=Y|AYs]) --> freeze_arg(X,Y), freeze_attrs(AXs,AYs).


ordering_saturate(I,O,O) :- \+(ordering_step(I,O,_)),!.
ordering_saturate(I,O1,O3) :-
	ordering_step(I,O1,O2),
	!,
	ordering_saturate(I,O2,O3).

step(X,Y,O) :- member(precedes(X,Y),O) ; member(strictlyPrecedes(X,Y),O).
path(X,X,_O,_Visited).
path(X,Y,O,Visited) :-
	step(X,Z,O),
	\+(member(Z,Visited)),
	path(Z,Y,O,[X|Visited]).
path(X,Y,O) :- path(X,Y,O,[]).

strict_cycle(O) :- member(strictlyPrecedes(X,Y),O),
	path(Y,X,O).

ordering_check(I) :-
	ordering_saturate(I,[],O),
	\+(strict_cycle(O)).


% Typing constraints
% Constraint 50.1
typing_step(I) -->
	{member(entity(E,_Attrs),I)},
	ensure(typeOf(E,entity)).

% Constraint 50.2
typing_step(I) -->
	{member(agent(Ag,_Attrs),I)},
	ensure(typeOf(Ag,agent)).

% Constraint 50.3
typing_step(I) -->
	{member(activity(A,_T1,_T2,_Attrs),I)},
	ensure(typeOf(A,activity)).

				% Disjunction in conclusion is correct here
				% because we can take a saturation step
				% iff either conclusion is not yet satisfied
	
% Constraint 50.4. 
typing_step(I) -->
	{member(used(_ID,A,E,_T,_Attrs),I)},
	(ensure(typeOf(A,activity)) ;
	 ensure(typeOf(E,entity)) ).

% Constraint 50.5.  
typing_step(I) -->
	{member(wasGeneratedBy(_ID,E,A,_T,_Attrs),I)},
	(ensure(typeOf(A,activity)) ;
	 ensure(typeOf(E,entity)) ).

% Constraint 50.6
typing_step(I) --> 
  {member(wasInformedBy(_ID, A2,A1,_Attrs),I)},
  (ensure(typeOf(A1,activity)) ;
   ensure(typeOf(A2,activity))).

% Constraint 50.7
typing_step(I) --> 
  {member(wasStartedBy(_ID, A2,E,A1,_Attrs),I)},
  (ensure(typeOf(A1,activity)) ;
   ensure(typeOf(E,entity)) ;
   ensure(typeOf(A2,activity))).

% Constraint 50.8
typing_step(I) --> 
  {member(wasEndedBy(_ID, A2,E,A1,_Attrs),I)},
  (ensure(typeOf(A1,activity)) ;
   ensure(typeOf(E,entity)) ;
   ensure(typeOf(A2,activity))).

% Constraint 50.9.  
typing_step(I) -->
	{member(wasInvalidatedBy(_ID,E,A,_T,_Attrs),I)},
	(ensure(typeOf(A,activity)) ;
	 ensure(typeOf(E,entity)) ).


% Constraint 50.10.
typing_step(I) -->
	{member(wasDerivedFrom(_Id,E2,E1,A,Gen,Use,_Attrs),I),
	 A == null, Gen == null, Use == null},
	(ensure(typeOf(E1,entity)) ;
	 ensure(typeOf(E2,entity)) ).

% Constraint 50.11.
typing_step(I) -->
	{member(wasDerivedFrom(_Id,E2,E1,A,Gen,Use,_Attrs),I),
	 A \== null, Gen \== null, Use \== null},
	(ensure(typeOf(E1,entity)) ;
	 ensure(typeOf(E2,entity)) ;
	 ensure(typeOf(A,activity)) ).


% Constraint 50.12.  
typing_step(I) -->
	{member(wasAttributedTo(_ID,E,Ag,_Attrs),I)},
	(ensure(typeOf(Ag,agent)) ;
	 ensure(typeOf(E,entity)) ).


% Constraint 50.13.
typing_step(I) -->
	{member(wasAssociatedWith(_Id,A,Ag,Pl,_Attrs),I),
	 Pl \== null},
	(ensure(typeOf(A,activity)) ;
	 ensure(typeOf(Ag,agent)) ;
	 ensure(typeOf(Pl,entity)) ).

% Constraint 50.14.
typing_step(I) -->
	{member(wasAssociatedWith(_Id,A,Ag,null,_Attrs),I)},
	(ensure(typeOf(A,activity)) ;
	 ensure(typeOf(Ag,agent)) ).

% Constraint 50.15.  
typing_step(I) -->
	{member(actedOnBehalfOf(_ID,Ag2,Ag1,A,_Attrs),I)},
	(ensure(typeOf(A,activity)) ;
	 ensure(typeOf(Ag1,agent)) ;
	 ensure(typeOf(Ag2,agent)) ).

% Constraint 50.16.  
typing_step(I) -->
	{member(alternateOf(E1,E2),I)},
	(ensure(typeOf(E1,entity)) ;
	 ensure(typeOf(E2,entity)) ).

% Constraint 50.17
typing_step(I) -->
	{member(specializationOf(E1,E2),I)},
	(ensure(typeOf(E1,entity)) ;
	 ensure(typeOf(E2,entity)) ).

% Constraint 50.18
typing_step(I) -->
	{member(hadMember(C,E),I)},
	(ensure(typeOf(C,'prov:Collection')) ;
	 ensure(typeOf(C,'entity')) ;
	 ensure(typeOf(E,entity)) ).

% Constraint 50.19
typing_step(I) -->
	{member(entity(C,Attrs),I),
	 member(Prov:'type'='prov:EmptyCollection',Attrs),
         ns(prov,Prov)},
	(ensure(typeOf(C,'prov:Collection')) ;
	 ensure(typeOf(C,'entity')) ;
	 ensure(typeOf(C,'prov:EmptyCollection')) ).



% TODO: More typing constraints


typing_saturate(I,T,T) :- \+(typing_step(I,T,_)).
typing_saturate(I,T1,T3) :- typing_step(I,T1,T2), !, typing_saturate(I,T2,T3).

typing(I,T) :- typing_saturate(I,[],T).

% Impossibility constraints

% Some background info
object(entity).
object(activity).
object(agent).
relation(used).
relation(wasGeneratedBy).
relation(wasInvalidatedBy).
relation(wasStartedBy).
relation(wasEndedBy).
relation(wasInformedBy).
relation(wasAttributedTo).
relation(wasAssociatedWith).
relation(actedOnBehalfOf).

% Constraint 51
impos(I,_T,c51_1) :- member(wasDerivedFrom(_Id,_E2,_E1,A,G,U,_Attrs),I),
	A == null, G \== null, U == null.
impos(I,_T,c51_2) :- member(wasDerivedFrom(_Id,_E2,_E1,A,G,U,_Attrs),I),
	A == null, G == null, U \== null.
impos(I,_T,c51_3) :- member(wasDerivedFrom(_Id,_E2,_E1,A,G,U,_Attrs),I),
	A == null, G \== null, U \== null.



% Constraint 52.  Have to be careful not to unify.
impos(I,_T,c52) :- member(specializationOf(E,E),I).

% Constraint 53.  
impos(I,_T,c53) :- member(T1,I),
	member(T2,I),
	T1 =.. [R,Id|_],
	T2 =.. [S,Id|_],
	relation(R),
	relation(S),
	R \== S.

%Constraint 54
impos(I,_T,c54) :- member(T1,I),
	member(T2,I),
	T1 =.. [P,Id|_],
	T2 =.. [R,Id|_],
	object(P),
	relation(R).

% Constraint 55
impos(_I,T,c55) :- member(typeOf(Id,entity),T),
	member(typeOf(Id,activity),T).

% Constraint 56
impos(I,T,c56) :- member(hadMember(C,_E1),I),
	member(typeOf(C,'prov:EmptyCollection'),T).
		     

impos_check(I,T) :- \+(impos(I,T,_)).

% Main body of validator: normalization, validation.

normalize(I,I) :- \+(infer(I,_)), !.
normalize(I,K) :- infer(I,J), !, normalize(J,K).


normalize_document((I,Bundles),(J,Bundles2)) :- 
  normalize(I,J),
  normalize_bundles(Bundles,Bundles2).

normalize_bundles([],[]).
normalize_bundles([(ID,J)|Bundles],[(ID,K)|Bundles2]) :- 
  normalize(J,K),
  normalize_bundles(Bundles,Bundles2).

valid(I) :- normalize(I,J),
	J \= invalid(_),
	freeze(J,K,0,_),
	ordering_check(K),
	typing(J,T),
	impos_check(J,T).


valid_document((I,Bundles)) :- 
  valid(I),
  valid_bundles(Bundles).

valid_bundles([]).
valid_bundles([(ID,J)|Bundles]) :- 
  valid(J),
  \+(member((ID,_),Bundles)),
  valid_bundles(Bundles).
