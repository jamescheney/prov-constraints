
%----------------------------------------------------------------------------
% Convert rules from the decl/4 clauses in constraints.pl
% to executable infer/2 clauses.
%  ... demonstrates approach but not finished.
%
% stephen.cresswell@tso.co.uk
%----------------------------------------------------------------------------

test_dump:-
	tell('generated_infer.pl'),
	findall(Clause/Label,some_clause(Clause,Label),Clauses),
	write_clauses(Clauses),
	told.

write_clauses([]).
write_clauses([C/label(Num,Name,Sub)|Cs]):-
	format('% ~w.~w ~w~n',[Num,Sub,Name]),
	current_output(Out),
	portray_clause(Out,C,[quoted(false)]),nl,
	write_clauses(Cs).

% Bind Clause to some infer/2 clause 
% derived from decl
some_clause(Clause,Label):-
	% inference rulea
	some_rule('Inference',Rule,Label),
	gen_rule(Rule,Erule),
	inference_rule_clause(Erule,Clause).
some_clause(Clause,Label):-
	% key constraint
	some_key(Key,Label),
	key_clause(Key,Clause).
some_clause(Clause,Label):-
	% constraint with two conditions on LHS and a single equality on RHS
	some_constraint_eq(Rule,Label),
	constraint_eq_clause(Rule,Clause).
some_clause(Clause,Label):-
	% precedence constraint
	some_precedence_constraint(Rule,Label),
	gen_rule(Rule,Erule),
	precedence_constraint_clause(Erule,Clause).
	
%----------------------------------------------------------------------------
% 'Inference' rules adding conclusion to assertions
%----------------------------------------------------------------------------

inference_rule_clause(erule(LHS,_PairsE,RHS),Clause):-
	condition_tests(LHS,I,Ps),
	condition_tests(RHS,I,CTs),
	append(RHS,I,RHS1),
	list_to_conj(CTs,CTConj),
	append(Ps,[\+(CTConj),J=RHS1],BodyList),
	list_to_conj(BodyList,BodyConj),
	Clause=( infer(I,J):-BodyConj ),
	I='I', % bind vars to fake names
	J='J'.	

condition_tests([],_I,[]).
condition_tests([C|Cs],I,[member(C,I)|Ts]):-
	condition_tests(Cs,I,Ts).

% Convert from grounded defn. of rule
% to version where atomic variable names are 
% mapped to unbound Prolog vars.
% The list of RHS existentially q'fied vars are
% preserved, because surely they will need to be bound
% to new atoms at some point?
gen_rule(rule(Uni,LHS,Exi,RHS),erule(LHS1,ExiVars,RHS1)):-
	findall(VName-_,(member(VName,Uni)),PairsU),
	findall(VName-_,(member(VName,Exi)),PairsE),
	append(PairsE,PairsU,PairsAll),
	%
	vars_bound_to_names(PairsAll),
	%
	subst_list(LHS,PairsU,LHS1),
	subst_list(RHS,PairsAll,RHS1),
	subst_list(Exi,PairsE,ExiVars).

some_rule(Kind,Rule,label(Num,Name,Subnum)):-
	decl(Kind,Num,Name,Body),
	rule_def(Body,Rules,[]),
	nth1(Subnum,Rules,Rule).

/*
% Individual rule/4 terms may occur directly or a list inside rules/1.
rule_contents(comment(_,Body),Sub):-
	rule_contents(Body,Sub).
rule_contents(rule(A,B,C,D),rule(A,B,C,D),'').
rule_contents(rule(A,B,D),rule(A,B,[],D),'').
rule_contents(rules(Rules),R,Subnum):-
	% filter list for rules only
	findall(rule(A,B,C,D),
	        (member(rule(A,B,D),Rules),C=[];member(rule(A,B,C,D),Rules)),
		Rules1),
	% ... then the position in the list is part of label
	nth1(Subnum,Rules1,R).
*/

rule_def(rule(A,B,D),[rule(A,B,[],D)|Rs],Rs):-!.
rule_def(R,[R|Rs],Rs):-
	R=rule(_,_,_,_),!.
rule_def(comment(_,Body),Rs0,Rs1):-
	!,
	rule_def(Body,Rs0,Rs1).
rule_def(rules(Rules),Rs0,Rs1):-
	rule_def_list(Rules,Rs0,Rs1).

rule_def_list([],Rs,Rs).
rule_def_list([R|Rs],Rs0,Rs2):-
	rule_def(R,Rs0,Rs1),
	rule_def_list(Rs,Rs1,Rs2).

%----------------------------------------------------------------------------
% Key constraints
%----------------------------------------------------------------------------

some_key(key(Arg,Statement),label(Num,Name,Subnum)):-
	decl('Constraint',Num,Name,rules(Rules)),
	findall(K,(K=key(_,_),member(K,Rules)),Keys),
	nth1(Subnum,Keys,key(Arg,Statement)).

% Assuming variable doesn't occur multiple times in same statement
key_template( key(V,Statement),Var,Suffix,Template):-
	Statement=..[F|Args],
	findall(Arg-_,member(Arg,Args),Pairs),
	memberchk(V-Var,Pairs), % usually V=id
	%
	vars_bound_to_names_with_suffix(Pairs,Args,Suffix),
	%
	subst_list(Args,Pairs,Blanks),
	Template=..[F|Blanks].

% A bit pointless, as we could just generate code 
%  Vars1 = Vars2
% but let's generate code in exactly the requested form
vars_unifier_expression_list([],[],[]).
vars_unifier_expression_list([V1|Vs1],[V2|Vs2],[V1=V2|Es]):-
	vars_unifier_expression_list(Vs1,Vs2,Es).


% Bind Clause to key constraint clause in the style of checker.pl
key_clause(Key,Clause):-
	% Two templates with different Prolog Vars
	key_template(Key,_,1,Temp1),
	key_template(Key,_,2,Temp2),
	Conds= [multimember(Temp1,Temp2,I),Var1==Var2,Cond1],
	Temp1=..[F,Var1|Args1],
	Temp2=..[F,Var2|Args2],
	append(RestArgs1,[Attrs1],Args1),
	append(RestArgs2,[Attrs2],Args2),
	append(RestArgs1,[Attrs3],Args3),
	Temp3=..[F,Var1|Args3],
	vars_unifier_expression_list(RestArgs1,RestArgs2,Elist),
	list_to_conj(Elist,Econj),
	Cond0=((Econj->(
	   remove(Temp1,I,J1),
	   remove(Temp2,J1,J2)),
	   append(Attrs1,Attrs2,Attrs3),
	   J=[Temp3|J2])
         ; J=invalid ),
	simpler_cond(Cond0,Cond1),
	list_to_conj(Conds,Body),
	Clause=(infer(I,J):-Body),
	I='I',J='J',J2='J2'. % BInd vars to their names
	
simpler_cond( (true->A;_), A):-!.
simpler_cond( A, A).

%----------------------------------------------------------------------------
% Constraint rules
%----------------------------------------------------------------------------
% Pattern where we have two conditions, and they both the same relation
% Since in this case, we're being sensitive about checking everything
% before unifying, we map args to prolog vars separately for each condition.

some_constraint_eq(Rule,Label):-
	Rule=rule(_Uni,[_,_],[],[_=_]),
	some_rule('Constraint',Rule,Label).

constraint_eq_clause(rule(Uni,[P1,P2],[],Conc),Clause):-
	P1=..[_|Args1],
	P2=..[_|Args2],
	findall(Vname-_,(member(Vname,Uni),memberchk(Vname,Args1)),Pairs1),
	findall(Vname-_,(member(Vname,Uni),memberchk(Vname,Args2)),Pairs2),
	findall(Arg,(member(Arg,Args1),memberchk(Arg,Args2)),Shared),
	eqs_for_shared_vars(Shared,Pairs1,Pairs2,Eqs),
	subst(P1,Pairs1,Template1),
	subst(P2,Pairs2,Template2),
	append(Pairs1,Pairs2,Pairs3),
	%
	vars_bound_to_names_with_suffix(Pairs1,Shared,1),
	vars_bound_to_names_with_suffix(Pairs2,Shared,2),
	%
	subst(Conc,Pairs3,[XV1=XV2]),
	list_to_conj(Eqs,Eq_conj),
	Clause=
          (infer(I,J):-multimember(Template1,Template2,I),
	     Eq_conj, XV1\==XV2,
	     (XV1=XV2 -> J=I; J=invalid)).

eqs_for_shared_vars([],_,_,[]).
eqs_for_shared_vars([Vname|Vnames],Pairs1,Pairs2,[V1==V2|Vs]):-
	memberchk(Vname-V1,Pairs1),
	memberchk(Vname-V2,Pairs2),
	eqs_for_shared_vars(Vnames,Pairs1,Pairs2,Vs).
	
%----------------------------------------------------------------------------
% Precedence constraints
%----------------------------------------------------------------------------

some_precedence_constraint(rule(Uni,Ps,[],[P1]),Label):-
	some_rule('Constraint',rule(Uni,Ps,[],[P0]),Label),
	precedence_rel(P0,P1).

precedence_rel( preceq(X,Y), precedes(X,Y) ).
precedence_rel( prec(X,Y), strictlyPrecedes(X,Y) ).

precedence_constraint_clause(erule(LHS,_PairsE,RHS),Clause):-
	condition_tests(LHS,I,Ps),
	condition_tests(RHS,O1,CTs),
	append(RHS,O1,RHS1),
	list_to_conj(CTs,CTConj),
	append(Ps,[\+(CTConj),O2=RHS1],BodyList),
	list_to_conj(BodyList,BodyConj),
	Clause=( ordering_step(I,O1,O2):-BodyConj ),
	I='I',O1='O1',O2='O2'.

%----------------------------------------------------------------------------
%----------------------------------------------------------------------------

% subst_list(+Term0,+Pairs,-Term1)
%  replace atoms in Term0 according to mapping in Pairs to bind Term1
%  Pairs is list of form [[old-new],...]
subst_list([],_,[]).
subst_list([T0|Ts0],Pairs,[T1|Ts1]):-
	subst(T0,Pairs,T1),
	subst_list(Ts0,Pairs,Ts1).

subst(T0,Pairs,Var):-
	atom(T0),
	memberchk(T0-Var,Pairs),
	!.
subst(T0,Pairs,T1):-
	T0=..[F|Ts0],!,
	subst_list(Ts0,Pairs,Ts1),
	T1=..[F|Ts1].
subst(T,_,T).
	
% map between [a,b,c] and (a,b,c)
list_to_conj([],true).
list_to_conj([H],H):-!.
list_to_conj([A|B0],(A,B1)):-
	list_to_conj(B0,B1).


%----------------------------------------------------------------------------
% Create fake variables for output of clauses by binding vars to their
% intended names
%----------------------------------------------------------------------------

vars_bound_to_names([]).
vars_bound_to_names([Name-Var|Pairs]):-
	fake_variable(Name,Var),
	vars_bound_to_names(Pairs).

% Fill bind variables in pairs to atoms to represent variable names
% For names in list Shared, the names are modified with Suffix
% This is needed to prevent unification too early where there
% are shared variables between rule conditions.
%  vars_bound_to_names_with_suffix(?Pairs,+Shared,+Suffix):-
vars_bound_to_names_with_suffix([],_,_).
vars_bound_to_names_with_suffix([Name-Var|Pairs],Shared,Suffix):-
	memberchk(Name,Shared),
	!,
	fake_variable(Name,Name1),
	format(atom(Var),'~w_~w',[Name1,Suffix]),
	vars_bound_to_names_with_suffix(Pairs,Shared,Suffix).
vars_bound_to_names_with_suffix([Name-Var|Pairs],Shared,Suffix):-
	fake_variable(Name,Var),
	vars_bound_to_names_with_suffix(Pairs,Shared,Suffix).

fake_variable( Na0, Na1 ):-
        name( Na0, [C0|Ns0] ),
        upcase( C0, C1 ),
	clean_string( Ns0, Ns1),
        name( Na1, [C1|Ns1] ).

upcase( C0, C1 ):-
        name(azA,[Ca,Cz,CA]),
        C0 >= Ca,
        C0 =< Cz,!,
        C1 is C0-Ca+CA.
upcase( C, C ).

% For presentation, some variable name atoms have trailing quote
% (as prime). We replace that with an underscore.

clean_string([],[]).
clean_string([39|Cs0],[95|Cs1]):- % 39='\'', 95='_'
	!,
	clean_string(Cs0,Cs1).
clean_string([C|Cs0],[C|Cs1]):-
	clean_string(Cs0,Cs1).
	
	
