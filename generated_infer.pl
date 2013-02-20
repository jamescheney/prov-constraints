% 5.1 communication-generation-use-inference
infer(I, J) :-
	member(wasInformedBy(Id, A_2, A_1, Attrs), I),
	\+ ( member(wasGeneratedBy(Gen, E, A_1, T_1, []), I),
	     member(used(Use, A_2, E, T_2, []), I)
	   ),
	J=[wasGeneratedBy(Gen, E, A_1, T_1, []), used(Use, A_2, E, T_2, [])|I].

% 6.1 generation-use-communication-inference
infer(I, J) :-
	member(wasGeneratedBy(Gen, E, A_1, T_1, Attrs_1), I),
	member(used(Id_2, A_2, E, T_2, Attrs_2), I),
	\+ member(wasInformedBy(Id, A_2, A_1, []), I),
	J=[wasInformedBy(Id, A_2, A_1, [])|I].

% 7.1 entity-generation-invalidation-inference
infer(I, J) :-
	member(entity(E, Attrs), I),
	\+ ( member(wasGeneratedBy(Gen, E, A_1, T_1, []), I),
	     member(wasInvalidatedBy(Inv, E, A_2, T_2, []), I)
	   ),
	J=[wasGeneratedBy(Gen, E, A_1, T_1, []), wasInvalidatedBy(Inv, E, A_2, T_2, [])|I].

% 8.1 activity-start-end-inference
infer(I, J) :-
	member(activity(A, T_1, T_2, Attrs), I),
	\+ ( member(wasStartedBy(Start, A, E_1, A_1, T_1, []), I),
	     member(wasEndedBy(End, A, E_2, A_2, T_2, []), I)
	   ),
	J=[wasStartedBy(Start, A, E_1, A_1, T_1, []), wasEndedBy(End, A, E_2, A_2, T_2, [])|I].

% 9.1 wasStartedBy-inference
infer(I, J) :-
	member(wasStartedBy(Id, A, E_1, A_1, T, Attrs), I),
	\+ member(wasGeneratedBy(Gen, E_1, A_1, T_1, []), I),
	J=[wasGeneratedBy(Gen, E_1, A_1, T_1, [])|I].

% 10.1 wasEndedBy-inference
infer(I, J) :-
	member(wasEndedBy(Id, A, E_1, A_1, T, Attrs), I),
	\+ member(wasGeneratedBy(Gen, E_1, A_1, T_1, []), I),
	J=[wasGeneratedBy(Gen, E_1, A_1, T_1, [])|I].

% 11.1 derivation-generation-use-inference
infer(I, J) :-
	member(notNull(A), I),
	member(notNull(Gen_2), I),
	member(notNull(Use_1), I),
	member(wasDerivedFrom(Id, E_2, E_1, A, Gen_2, Use_1, Attrs), I),
	\+ ( member(used(Use_1, A, E_1, T_1, []), I),
	     member(wasGeneratedBy(Gen_2, E_2, A, T_2, []), I)
	   ),
	J=[used(Use_1, A, E_1, T_1, []), wasGeneratedBy(Gen_2, E_2, A, T_2, [])|I].

% 12.1 revision-is-alternate-inference
infer(I, J) :-
	member(wasDerivedFrom(Id, E_2, E_1, A, G, U, [prov:type = prov:Revision]), I),
	\+ member(alternateOf(E_2, E_1), I),
	J=[alternateOf(E_2, E_1)|I].

% 13.1 attribution-inference
infer(I, J) :-
	member(wasAttributedTo(Att, E, Ag, Attrs), I),
	\+ ( member(wasGeneratedBy(Gen, E, A, T, []), I),
	     member(wasAssociatedWith(Assoc, A, Ag, Pl, []), I)
	   ),
	J=[wasGeneratedBy(Gen, E, A, T, []), wasAssociatedWith(Assoc, A, Ag, Pl, [])|I].

% 14.1 delegation-inference
infer(I, J) :-
	member(actedOnBehalfOf(Id, Ag_1, Ag_2, A, Attrs), I),
	\+ ( member(wasAssociatedWith(Id_1, A, Ag_1, Pl_1, []), I),
	     member(wasAssociatedWith(Id_2, A, Ag_2, Pl_2, []), I)
	   ),
	J=[wasAssociatedWith(Id_1, A, Ag_1, Pl_1, []), wasAssociatedWith(Id_2, A, Ag_2, Pl_2, [])|I].

% 15.1 influence-inference
infer(I, J) :-
	member(wasGeneratedBy(Id, E, A, T, Attrs), I),
	\+ member(wasInfluencedBy(Id, E, A, Attrs), I),
	J=[wasInfluencedBy(Id, E, A, Attrs)|I].

% 15.2 influence-inference
infer(I, J) :-
	member(used(Id, A, E, T, Attrs), I),
	\+ member(wasInfluencedBy(Id, A, E, Attrs), I),
	J=[wasInfluencedBy(Id, A, E, Attrs)|I].

% 15.3 influence-inference
infer(I, J) :-
	member(wasInformedBy(Id, A_2, A_1, Attrs), I),
	\+ member(wasInfluencedBy(Id, A_2, A_1, Attrs), I),
	J=[wasInfluencedBy(Id, A_2, A_1, Attrs)|I].

% 15.4 influence-inference
infer(I, J) :-
	member(wasStartedBy(Id, A_2, E, A_1, T, Attrs), I),
	\+ member(wasInfluencedBy(Id, A_2, E, Attrs), I),
	J=[wasInfluencedBy(Id, A_2, E, Attrs)|I].

% 15.5 influence-inference
infer(I, J) :-
	member(wasEndedBy(Id, A_2, E, A_1, T, Attrs), I),
	\+ member(wasInfluencedBy(Id, A_2, E, Attrs), I),
	J=[wasInfluencedBy(Id, A_2, E, Attrs)|I].

% 15.6 influence-inference
infer(I, J) :-
	member(wasInvalidatedBy(Id, E, A, T, Attrs), I),
	\+ member(wasInfluencedBy(Id, E, A, Attrs), I),
	J=[wasInfluencedBy(Id, E, A, Attrs)|I].

% 15.7 influence-inference
infer(I, J) :-
	member(wasDerivedFrom(Id, E_2, E_1, A, G, U, Attrs), I),
	\+ member(wasInfluencedBy(Id, E_2, E_1, Attrs), I),
	J=[wasInfluencedBy(Id, E_2, E_1, Attrs)|I].

% 15.8 influence-inference
infer(I, J) :-
	member(wasAttributedTo(Id, E, Ag, Attrs), I),
	\+ member(wasInfluencedBy(Id, E, Ag, Attrs), I),
	J=[wasInfluencedBy(Id, E, Ag, Attrs)|I].

% 15.9 influence-inference
infer(I, J) :-
	member(wasAssociatedWith(Id, A, Ag, Pl, Attrs), I),
	\+ member(wasInfluencedBy(Id, A, Ag, Attrs), I),
	J=[wasInfluencedBy(Id, A, Ag, Attrs)|I].

% 15.10 influence-inference
infer(I, J) :-
	member(actedOnBehalfOf(Id, Ag_2, Ag_1, A, Attrs), I),
	\+ member(wasInfluencedBy(Id, Ag_2, Ag_1, Attrs), I),
	J=[wasInfluencedBy(Id, Ag_2, Ag_1, Attrs)|I].

% 16.1 alternate-reflexive
infer(I, J) :-
	member(entity(E), I),
	\+ member(alternateOf(E, E), I),
	J=[alternateOf(E, E)|I].

% 17.1 alternate-transitive
infer(I, J) :-
	member(alternateOf(E_1, E_2), I),
	member(alternateOf(E_2, E_3), I),
	\+ member(alternateOf(E_1, E_3), I),
	J=[alternateOf(E_1, E_3)|I].

% 18.1 alternate-symmetric
infer(I, J) :-
	member(alternateOf(E_1, E_2), I),
	\+ member(alternateOf(E_2, E_1), I),
	J=[alternateOf(E_2, E_1)|I].

% 19.1 specialization-transitive
infer(I, J) :-
	member(specializationOf(E_1, E_2), I),
	member(specializationOf(E_2, E_3), I),
	\+ member(specializationOf(E_1, E_3), I),
	J=[specializationOf(E_1, E_3)|I].

% 20.1 specialization-alternate-inference
infer(I, J) :-
	member(specializationOf(E_1, E_2), I),
	\+ member(alternateOf(E_1, E_2), I),
	J=[alternateOf(E_1, E_2)|I].

% 21.1 specialization-attributes-inference
infer(I, J) :-
	member(entity(E_1, Attrs), I),
	member(specializationOf(E_2, E_1), I),
	\+ member(entity(E_2, Attrs), I),
	J=[entity(E_2, Attrs)|I].

% 22.1 key-object
infer(I, J) :-
	multimember(entity(Id_1, Attrs_1), entity(Id_2, Attrs_2), I),
	Id_1==Id_2,
	( remove(entity(Id_1, Attrs_1), I, A),
	  remove(entity(Id_2, Attrs_2), A, J2)
	),
	append(Attrs_1, Attrs_2, B),
	J=[entity(Id_1, B)|J2].

% 22.2 key-object
infer(I, J) :-
	multimember(activity(Id_1, T1_1, T2_1, Attrs_1),
		    activity(Id_2, T1_2, T2_2, Attrs_2),
		    I),
	Id_1==Id_2,
	(   T1_1=T1_2,
	    T2_1=T2_2
	->  ( remove(activity(Id_1, T1_1, T2_1, Attrs_1), I, A),
	      remove(activity(Id_2, T1_2, T2_2, Attrs_2), A, J2)
	    ),
	    append(Attrs_1, Attrs_2, B),
	    J=[activity(Id_1, T1_1, T2_1, B)|J2]
	;   J=invalid
	).

% 22.3 key-object
infer(I, J) :-
	multimember(agent(Id_1, Attrs_1), agent(Id_2, Attrs_2), I),
	Id_1==Id_2,
	( remove(agent(Id_1, Attrs_1), I, A),
	  remove(agent(Id_2, Attrs_2), A, J2)
	),
	append(Attrs_1, Attrs_2, B),
	J=[agent(Id_1, B)|J2].

% 23.1 key-properties
infer(I, J) :-
	multimember(wasGeneratedBy(Id_1, E_1, A_1, T_1, Attrs_1),
		    wasGeneratedBy(Id_2, E_2, A_2, T_2, Attrs_2),
		    I),
	Id_1==Id_2,
	(   E_1=E_2,
	    A_1=A_2,
	    T_1=T_2
	->  ( remove(wasGeneratedBy(Id_1, E_1, A_1, T_1, Attrs_1), I, A),
	      remove(wasGeneratedBy(Id_2, E_2, A_2, T_2, Attrs_2), A, J2)
	    ),
	    append(Attrs_1, Attrs_2, B),
	    J=[wasGeneratedBy(Id_1, E_1, A_1, T_1, B)|J2]
	;   J=invalid
	).

% 23.2 key-properties
infer(I, J) :-
	multimember(used(Id_1, A_1, E_1, T_1, Attrs_1),
		    used(Id_2, A_2, E_2, T_2, Attrs_2),
		    I),
	Id_1==Id_2,
	(   A_1=A_2,
	    E_1=E_2,
	    T_1=T_2
	->  ( remove(used(Id_1, A_1, E_1, T_1, Attrs_1), I, A),
	      remove(used(Id_2, A_2, E_2, T_2, Attrs_2), A, J2)
	    ),
	    append(Attrs_1, Attrs_2, B),
	    J=[used(Id_1, A_1, E_1, T_1, B)|J2]
	;   J=invalid
	).

% 23.3 key-properties
infer(I, J) :-
	multimember(wasInformedBy(Id_1, A_2_1, A_1_1, Attrs_1),
		    wasInformedBy(Id_2, A_2_2, A_1_2, Attrs_2),
		    I),
	Id_1==Id_2,
	(   A_2_1=A_2_2,
	    A_1_1=A_1_2
	->  ( remove(wasInformedBy(Id_1, A_2_1, A_1_1, Attrs_1), I, A),
	      remove(wasInformedBy(Id_2, A_2_2, A_1_2, Attrs_2), A, J2)
	    ),
	    append(Attrs_1, Attrs_2, B),
	    J=[wasInformedBy(Id_1, A_2_1, A_1_1, B)|J2]
	;   J=invalid
	).

% 23.4 key-properties
infer(I, J) :-
	multimember(wasStartedBy(Id_1, A_2_1, E_1, A_1_1, T_1, Attrs_1),
		    wasStartedBy(Id_2, A_2_2, E_2, A_1_2, T_2, Attrs_2),
		    I),
	Id_1==Id_2,
	(   A_2_1=A_2_2,
	    E_1=E_2,
	    A_1_1=A_1_2,
	    T_1=T_2
	->  ( remove(wasStartedBy(Id_1, A_2_1, E_1, A_1_1, T_1, Attrs_1), I, A),
	      remove(wasStartedBy(Id_2, A_2_2, E_2, A_1_2, T_2, Attrs_2),
		     A,
		     J2)
	    ),
	    append(Attrs_1, Attrs_2, B),
	    J=[wasStartedBy(Id_1, A_2_1, E_1, A_1_1, T_1, B)|J2]
	;   J=invalid
	).

% 23.5 key-properties
infer(I, J) :-
	multimember(wasEndedBy(Id_1, A_2_1, E_1, A_1_1, T_1, Attrs_1),
		    wasEndedBy(Id_2, A_2_2, E_2, A_1_2, T_2, Attrs_2),
		    I),
	Id_1==Id_2,
	(   A_2_1=A_2_2,
	    E_1=E_2,
	    A_1_1=A_1_2,
	    T_1=T_2
	->  ( remove(wasEndedBy(Id_1, A_2_1, E_1, A_1_1, T_1, Attrs_1), I, A),
	      remove(wasEndedBy(Id_2, A_2_2, E_2, A_1_2, T_2, Attrs_2), A, J2)
	    ),
	    append(Attrs_1, Attrs_2, B),
	    J=[wasEndedBy(Id_1, A_2_1, E_1, A_1_1, T_1, B)|J2]
	;   J=invalid
	).

% 23.6 key-properties
infer(I, J) :-
	multimember(wasInvalidatedBy(Id_1, E_1, A_1, T_1, Attrs_1),
		    wasInvalidatedBy(Id_2, E_2, A_2, T_2, Attrs_2),
		    I),
	Id_1==Id_2,
	(   E_1=E_2,
	    A_1=A_2,
	    T_1=T_2
	->  ( remove(wasInvalidatedBy(Id_1, E_1, A_1, T_1, Attrs_1), I, A),
	      remove(wasInvalidatedBy(Id_2, E_2, A_2, T_2, Attrs_2), A, J2)
	    ),
	    append(Attrs_1, Attrs_2, B),
	    J=[wasInvalidatedBy(Id_1, E_1, A_1, T_1, B)|J2]
	;   J=invalid
	).

% 23.7 key-properties
infer(I, J) :-
	multimember(wasDerivedFrom(Id_1, E_2_1, E_1_1, A_1, G2_1, U1_1, Attrs_1),
		    wasDerivedFrom(Id_2, E_2_2, E_1_2, A_2, G2_2, U1_2, Attrs_2),
		    I),
	Id_1==Id_2,
	(   E_2_1=E_2_2,
	    E_1_1=E_1_2,
	    A_1=A_2,
	    G2_1=G2_2,
	    U1_1=U1_2
	->  ( remove(wasDerivedFrom(Id_1, E_2_1, E_1_1, A_1, G2_1, U1_1, Attrs_1),
		     I,
		     A),
	      remove(wasDerivedFrom(Id_2, E_2_2, E_1_2, A_2, G2_2, U1_2, Attrs_2),
		     A,
		     J2)
	    ),
	    append(Attrs_1, Attrs_2, B),
	    J=[wasDerivedFrom(Id_1, E_2_1, E_1_1, A_1, G2_1, U1_1, B)|J2]
	;   J=invalid
	).

% 23.8 key-properties
infer(I, J) :-
	multimember(wasAttributedTo(Id_1, E_1, Ag_1, Attr_1),
		    wasAttributedTo(Id_2, E_2, Ag_2, Attr_2),
		    I),
	Id_1==Id_2,
	(   E_1=E_2,
	    Ag_1=Ag_2
	->  ( remove(wasAttributedTo(Id_1, E_1, Ag_1, Attr_1), I, A),
	      remove(wasAttributedTo(Id_2, E_2, Ag_2, Attr_2), A, J2)
	    ),
	    append(Attr_1, Attr_2, B),
	    J=[wasAttributedTo(Id_1, E_1, Ag_1, B)|J2]
	;   J=invalid
	).

% 23.9 key-properties
infer(I, J) :-
	multimember(wasAssociatedWith(Id_1, A_1, Ag_1, Pl_1, Attrs_1),
		    wasAssociatedWith(Id_2, A_2, Ag_2, Pl_2, Attrs_2),
		    I),
	Id_1==Id_2,
	(   A_1=A_2,
	    Ag_1=Ag_2,
	    Pl_1=Pl_2
	->  ( remove(wasAssociatedWith(Id_1, A_1, Ag_1, Pl_1, Attrs_1), I, A),
	      remove(wasAssociatedWith(Id_2, A_2, Ag_2, Pl_2, Attrs_2), A, J2)
	    ),
	    append(Attrs_1, Attrs_2, B),
	    J=[wasAssociatedWith(Id_1, A_1, Ag_1, Pl_1, B)|J2]
	;   J=invalid
	).

% 23.10 key-properties
infer(I, J) :-
	multimember(actedOnBehalfOf(Id_1, Ag_2_1, Ag_1_1, A_1, Attrs_1),
		    actedOnBehalfOf(Id_2, Ag_2_2, Ag_1_2, A_2, Attrs_2),
		    I),
	Id_1==Id_2,
	(   Ag_2_1=Ag_2_2,
	    Ag_1_1=Ag_1_2,
	    A_1=A_2
	->  ( remove(actedOnBehalfOf(Id_1, Ag_2_1, Ag_1_1, A_1, Attrs_1),
		     I,
		     A),
	      remove(actedOnBehalfOf(Id_2, Ag_2_2, Ag_1_2, A_2, Attrs_2),
		     A,
		     J2)
	    ),
	    append(Attrs_1, Attrs_2, B),
	    J=[actedOnBehalfOf(Id_1, Ag_2_1, Ag_1_1, A_1, B)|J2]
	;   J=invalid
	).

% 23.11 key-properties
infer(I, J) :-
	multimember(wasInfluencedBy(Id_1, O2_1, O1_1, Attrs_1),
		    wasInfluencedBy(Id_2, O2_2, O1_2, Attrs_2),
		    I),
	Id_1==Id_2,
	(   O2_1=O2_2,
	    O1_1=O1_2
	->  ( remove(wasInfluencedBy(Id_1, O2_1, O1_1, Attrs_1), I, A),
	      remove(wasInfluencedBy(Id_2, O2_2, O1_2, Attrs_2), A, J2)
	    ),
	    append(Attrs_1, Attrs_2, B),
	    J=[wasInfluencedBy(Id_1, O2_1, O1_1, B)|J2]
	;   J=invalid
	).

% 24.1 unique-generation
infer(A, B) :-
	multimember(wasGeneratedBy(Gen_1, E_1, A_1, T_1, Attrs_1),
		    wasGeneratedBy(Gen_2, E_2, A_2, T_2, Attrs_2),
		    A),
	( E_1==E_2,
	  A_1==A_2
	),
	Gen_1\==Gen_2,
	(   Gen_1=Gen_2
	->  B=A
	;   B=invalid
	).

% 25.1 unique-invalidation
infer(A, B) :-
	multimember(wasInvalidatedBy(Inv_1, E_1, A_1, T_1, Attrs_1),
		    wasInvalidatedBy(Inv_2, E_2, A_2, T_2, Attrs_2),
		    A),
	( E_1==E_2,
	  A_1==A_2
	),
	Inv_1\==Inv_2,
	(   Inv_1=Inv_2
	->  B=A
	;   B=invalid
	).

% 26.1 unique-wasStartedBy
infer(A, B) :-
	multimember(wasStartedBy(Start_1, A_1, E_1, A_0_1, T_1, Attrs_1),
		    wasStartedBy(Start_2, A_2, E_2, A_0_2, T_2, Attrs_2),
		    A),
	( A_1==A_2,
	  A_0_1==A_0_2
	),
	Start_1\==Start_2,
	(   Start_1=Start_2
	->  B=A
	;   B=invalid
	).

% 27.1 unique-wasEndedBy
infer(A, B) :-
	multimember(wasEndedBy(End_1, A_1, E_1, A_0_1, T_1, Attrs_1),
		    wasEndedBy(End_2, A_2, E_2, A_0_2, T_2, Attrs_2),
		    A),
	( A_1==A_2,
	  A_0_1==A_0_2
	),
	End_1\==End_2,
	(   End_1=End_2
	->  B=A
	;   B=invalid
	).

% 28.1 unique-startTime
infer(A, B) :-
	multimember(activity(A_2_1, T_1, T_2, Attrs),
		    wasStartedBy(Start, A_2_2, E, A_1, T, Attrs_1),
		    A),
	A_2_1==A_2_2,
	T_1\==T,
	(   T_1=T
	->  B=A
	;   B=invalid
	).

% 29.1 unique-endTime
infer(A, B) :-
	multimember(activity(A_2_1, T_1, T_2, Attrs),
		    wasEndedBy(End, A_2_2, E, A_1, T, Attrs_1),
		    A),
	A_2_1==A_2_2,
	T_2\==T,
	(   T_2=T
	->  B=A
	;   B=invalid
	).

% 30.1 start-precedes-end
ordering_step(I, O1, O2) :-
	member(wasStartedBy(Start, A, E_1, A_1, T_1, Attrs_1), I),
	member(wasEndedBy(End, A, E_2, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Start, End), O1),
	O2=[precedes(Start, End)|O1].

% 31.1 start-start-ordering
ordering_step(I, O1, O2) :-
	member(wasStartedBy(Start_1, A, E_1, A_1, T_1, Attrs_1), I),
	member(wasStartedBy(Start_2, A, E_2, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Start_1, Start_2), O1),
	O2=[precedes(Start_1, Start_2)|O1].

% 32.1 end-end-ordering
ordering_step(I, O1, O2) :-
	member(wasEndedBy(End_1, A, E_1, A_1, T_1, Attrs_1), I),
	member(wasEndedBy(End_2, A, E_2, A_2, T_2, Attrs_2), I),
	\+ member(precedes(End_1, end2), O1),
	O2=[precedes(End_1, end2)|O1].

% 33.1 usage-within-activity
ordering_step(I, O1, O2) :-
	member(wasStartedBy(Start, A, E_1, A_1, T_1, Attrs_1), I),
	member(used(Use, A, E_2, T_2, Attrs_2), I),
	\+ member(precedes(Start, Use), O1),
	O2=[precedes(Start, Use)|O1].

% 33.2 usage-within-activity
ordering_step(I, O1, O2) :-
	member(used(Use, A, E_1, T_1, Attrs_1), I),
	member(wasEndedBy(End, A, E_2, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Use, End), O1),
	O2=[precedes(Use, End)|O1].

% 34.1 generation-within-activity
ordering_step(I, O1, O2) :-
	member(wasStartedBy(Start, A, E_1, A_1, T_1, Attrs_1), I),
	member(wasGeneratedBy(Gen, E_2, A, T_2, Attrs_2), I),
	\+ member(precedes(Start, Gen), O1),
	O2=[precedes(Start, Gen)|O1].

% 34.2 generation-within-activity
ordering_step(I, O1, O2) :-
	member(wasGeneratedBy(Gen, E, A, T, Attrs), I),
	member(wasEndedBy(End, A, E_1, A_1, T_1, Attrs_1), I),
	\+ member(precedes(Gen, End), O1),
	O2=[precedes(Gen, End)|O1].

% 35.1 wasInformedBy-ordering
ordering_step(I, O1, O2) :-
	member(wasInformedBy(Id, A_2, A_1, Attrs), I),
	member(wasStartedBy(Start, A_1, E_1, A_1_, T_1, Attrs_1), I),
	member(wasEndedBy(End, A_2, E_2, A_2_, T_2, Attrs_2), I),
	\+ member(precedes(Start, End), O1),
	O2=[precedes(Start, End)|O1].

% 36.1 generation-precedes-invalidation
ordering_step(I, O1, O2) :-
	member(wasGeneratedBy(Gen, E, A_1, T_1, Attrs_1), I),
	member(wasInvalidatedBy(Inv, E, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Gen, Inv), O1),
	O2=[precedes(Gen, Inv)|O1].

% 37.1 generation-precedes-usage
ordering_step(I, O1, O2) :-
	member(wasGeneratedBy(Gen, E, A_1, T_1, Attrs_1), I),
	member(used(Use, A_2, E, T_2, Attrs_2), I),
	\+ member(precedes(Gen, Use), O1),
	O2=[precedes(Gen, Use)|O1].

% 38.1 usage-precedes-invalidation
ordering_step(I, O1, O2) :-
	member(used(Use, A_1, E, T_1, Attrs_1), I),
	member(wasInvalidatedBy(Inv, E, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Use, Inv), O1),
	O2=[precedes(Use, Inv)|O1].

% 39.1 generation-generation-ordering
ordering_step(I, O1, O2) :-
	member(wasGeneratedBy(Gen_1, E, A_1, T_1, Attrs_1), I),
	member(wasGeneratedBy(Gen_2, E, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Gen_1, Gen_2), O1),
	O2=[precedes(Gen_1, Gen_2)|O1].

% 40.1 invalidation-invalidation-ordering
ordering_step(I, O1, O2) :-
	member(wasInvalidatedBy(Inv_1, E, A_1, T_1, Attrs_1), I),
	member(wasInvalidatedBy(Inv_2, E, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Inv_1, Inv_2), O1),
	O2=[precedes(Inv_1, Inv_2)|O1].

% 41.1 derivation-usage-generation-ordering
ordering_step(I, O1, O2) :-
	member(notNull(A), I),
	member(notNull(Gen_2), I),
	member(notNull(Use_1), I),
	member(wasDerivedFrom(D, E_2, E_1, A, Gen_2, Use_1, Attrs), I),
	\+ member(precedes(Use_1, Gen_2), O1),
	O2=[precedes(Use_1, Gen_2)|O1].

% 42.1 derivation-generation-generation-ordering
ordering_step(I, O1, O2) :-
	member(wasDerivedFrom(D, E_2, E_1, A, G, U, Attrs), I),
	member(wasGeneratedBy(Gen_1, E_1, A_1, T_1, Attrs_1), I),
	member(wasGeneratedBy(Gen_2, E_2, A_2, T_2, Attrs_2), I),
	\+ member(strictlyPrecedes(Gen_1, Gen_2), O1),
	O2=[strictlyPrecedes(Gen_1, Gen_2)|O1].

% 43.1 wasStartedBy-ordering
ordering_step(I, O1, O2) :-
	member(wasGeneratedBy(Gen, E, A_1, T_1, Attrs_1), I),
	member(wasStartedBy(Start, A, E, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Gen, Start), O1),
	O2=[precedes(Gen, Start)|O1].

% 43.2 wasStartedBy-ordering
ordering_step(I, O1, O2) :-
	member(wasStartedBy(Start, A, E, A_1, T_1, Attrs_1), I),
	member(wasInvalidatedBy(Inv, E, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Start, Inv), O1),
	O2=[precedes(Start, Inv)|O1].

% 44.1 wasEndedBy-ordering
ordering_step(I, O1, O2) :-
	member(wasGeneratedBy(Gen, E, A_1, T_1, Attrs_1), I),
	member(wasEndedBy(End, A, E, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Gen, End), O1),
	O2=[precedes(Gen, End)|O1].

% 44.2 wasEndedBy-ordering
ordering_step(I, O1, O2) :-
	member(wasEndedBy(End, A, E, A_1, T_1, Attrs_1), I),
	member(wasInvalidatedBy(Inv, E, A_2, T_2, Attrs_2), I),
	\+ member(precedes(End, Inv), O1),
	O2=[precedes(End, Inv)|O1].

% 45.1 specialization-generation-ordering
ordering_step(I, O1, O2) :-
	member(specializationOf(E_2, E_1), I),
	member(wasGeneratedBy(Gen_1, E_1, A_1, T_1, Attrs_1), I),
	member(wasGeneratedBy(Gen_2, E_2, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Gen_1, Gen_2), O1),
	O2=[precedes(Gen_1, Gen_2)|O1].

% 46.1 specialization-invalidation-ordering
ordering_step(I, O1, O2) :-
	member(specializationOf(E_1, E_2), I),
	member(wasInvalidatedBy(Inv_1, E_1, A_1, T_1, Attrs_1), I),
	member(wasInvalidatedBy(Inv_2, E_2, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Inv_1, Inv_2), O1),
	O2=[precedes(Inv_1, Inv_2)|O1].

% 47.1 wasAssociatedWith-ordering
ordering_step(I, O1, O2) :-
	member(wasAssociatedWith(Assoc, a, Ag, pl, attrs), I),
	member(wasStartedBy(Start_1, a, E_1, A_1, T_1, Attrs_1), I),
	member(wasInvalidatedBy(Inv_2, Ag, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Start_1, Inv_2), O1),
	O2=[precedes(Start_1, Inv_2)|O1].

% 47.2 wasAssociatedWith-ordering
ordering_step(I, O1, O2) :-
	member(wasAssociatedWith(Assoc, a, Ag, pl, attrs), I),
	member(wasGeneratedBy(Gen_1, Ag, A_1, T_1, Attrs_1), I),
	member(wasEndedBy(End_2, a, E_2, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Gen_1, End_2), O1),
	O2=[precedes(Gen_1, End_2)|O1].

% 47.3 wasAssociatedWith-ordering
ordering_step(I, O1, O2) :-
	member(wasAssociatedWith(Assoc, a, Ag, pl, attrs), I),
	member(wasStartedBy(Start_1, a, E_1, A_1, T_1, Attrs_1), I),
	member(wasEndedBy(End_2, Ag, E_2, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Start_1, End_2), O1),
	O2=[precedes(Start_1, End_2)|O1].

% 47.4 wasAssociatedWith-ordering
ordering_step(I, O1, O2) :-
	member(wasAssociatedWith(Assoc, a, Ag, pl, attrs), I),
	member(wasStartedBy(Start_1, Ag, E_1, A_1, T_1, Attrs_1), I),
	member(wasEndedBy(End_2, a, E_2, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Start_1, End_2), O1),
	O2=[precedes(Start_1, End_2)|O1].

% 48.1 wasAttributedTo-ordering
ordering_step(I, O1, O2) :-
	member(wasAttributedTo(Att, E, Ag, Attrs), I),
	member(wasGeneratedBy(Gen_1, Ag, A_1, T_1, Attrs_1), I),
	member(wasGeneratedBy(Gen_2, E, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Gen_1, Gen_2), O1),
	O2=[precedes(Gen_1, Gen_2)|O1].

% 48.2 wasAttributedTo-ordering
ordering_step(I, O1, O2) :-
	member(wasAttributedTo(Att, E, Ag, Attrs), I),
	member(wasStartedBy(Start_1, Ag, E_1, A_1, T_1, Attrs_1), I),
	member(wasGeneratedBy(Gen_2, E, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Start_1, Gen_2), O1),
	O2=[precedes(Start_1, Gen_2)|O1].

% 49.1 actedOnBehalfOf-ordering
ordering_step(I, O1, O2) :-
	member(actedOnBehalfOf(Del, Ag_2, Ag_1, A, Attrs), I),
	member(wasGeneratedBy(Gen_1, Ag_1, A_1, T_1, Attrs_1), I),
	member(wasInvalidatedBy(Inv_2, Ag_2, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Gen_1, Inv_2), O1),
	O2=[precedes(Gen_1, Inv_2)|O1].

% 49.2 actedOnBehalfOf-ordering
ordering_step(I, O1, O2) :-
	member(actedOnBehalfOf(Del, Ag_2, Ag_1, A, Attrs), I),
	member(wasStartedBy(Start_1, Ag_1, E_1, A_1, T_1, Attrs_1), I),
	member(wasEndedBy(End_2, Ag_2, E_2, A_2, T_2, Attrs_2), I),
	\+ member(precedes(Start_1, End_2), O1),
	O2=[precedes(Start_1, End_2)|O1].

