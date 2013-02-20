%% The rules.
%% A decl is of the form
%% decl(Type,N,Name,Rules)
%% where Type is Inference or Constraint
%%       N is the inf/constraint number
%%       Name is the textual name
%%       Rules is the body of the rule.
%% A declaration body is of the form
%% Body ::= rule(Xs,Hyps,Ys,Concls)
%%        | rule(Xs,Hyps,Concls) % no existential vars
%%        | comment(Comment,Body)
%%        | rules([Body1,...,Bodyn])
%% A rule(Xs,Hyps,Ys,Body) is an implication
%% forall Xs. Hyps ==> exists Ys. Concls
%% A comment(Comment,Body) is a body with a surrounding comment
%%  - the body can be an atom or a list of atoms with math() fucntions
%%     the math() constructors are translated to appropriate
%%     delimiters of latex environments
%% A rules(L) is a list of rules, which will be formatted as a list.
%% Nested rules() are not guaranteed to work, but comments within list
%% elements are OK.


%% TODO: Recognize and do correct thing for special predicates
%% (equality, membership, precedes).  Currently we just represent these
%% as atoms.
%% TODO: Allow comments to have multiple representations, depending on the
%% target (HTML, LaTeX/Wiki)

decl('Inference', 5,'communication-generation-use-inference',
     rule([id, a_2,a_1,attrs],
	  [wasInformedBy(id, a_2,a_1,attrs)],
	  [e, gen, t_1, use, t_2],
	  [wasGeneratedBy(gen, e,a_1,t_1,[]), used(use, a_2,e,t_2,[])])).

decl('Inference', 6, 'generation-use-communication-inference',
     rule([gen, e, a_1, t_1, attrs_1, id_2, a_2, t_2, attrs_2],
	  [wasGeneratedBy(gen, e,a_1,t_1,attrs_1),
	   used(id_2, a_2,e,t_2,attrs_2)],
	  [id],
	  [wasInformedBy(id, a_2,a_1,[])])).

decl('Inference', 7, 'entity-generation-invalidation-inference',
     rule([e,attrs],
	  [entity(e,attrs)],
	  [gen,a_1,t_1,inv,a_2,t_2],
	  [wasGeneratedBy(gen,e,a_1,t_1,[]),
	   wasInvalidatedBy(inv,e,a_2,t_2,[])])).

decl('Inference', 8, 'activity-start-end-inference',
     rule([a, t_1, t_2, attrs],
	  [activity(a,t_1,t_2,attrs)],
	  [start, e_1, a_1, end, a_2, e_2],
	  [wasStartedBy(start, a,e_1,a_1,t_1,[]),
	   wasEndedBy(end, a,e_2,a_2,t_2,[])])).

decl('Inference', 9, 'wasStartedBy-inference',
     rule( [id, a, e_1, a_1, t, attrs],
	   [wasStartedBy(id, a,e_1,a_1,t,attrs)],
	   [gen, t_1],
	   [wasGeneratedBy(gen, e_1,a_1,t_1,[])])).


decl('Inference', 10, 'wasEndedBy-inference',
     rule( [id, a, e_1, a_1, t, attrs],
	   [wasEndedBy(id, a,e_1,a_1,t,attrs)],
	   [gen, t_1],
	   [wasGeneratedBy(gen, e_1,a_1,t_1,[])])).


decl('Inference', 11, 'derivation-generation-use-inference',
     comment(['In this inference, none of ',
	      math(a), ',', 
	      math(gen_2), ', or ', 
	      math(use_1),' can be placeholders -.'],
	     rule( [id, e_2, e_1, a, gen_2, use_1, attrs],
		   [notNull(a),
		    notNull(gen_2),
		    notNull(use_1),
		    wasDerivedFrom(id, e_2,e_1,a,gen_2,use_1,attrs)],
		   [t_1,t_2 ],
		   [used(use_1, a,e_1,t_1,[]),
		    wasGeneratedBy(gen_2, e_2,a,t_2,[])]))).
    

decl('Inference', 12, 'revision-is-alternate-inference', 
     comment(['In this inference, any of ',
	      math(a), ',', 
	      math(gen_2), ', or ', 
	      math(use_1),' can be placeholders -.'],
     rule( [id, e_1, e_2, a, g,u],
	   [wasDerivedFrom(id, e_2,e_1,a,g,u,['prov:type = prov:Revision'])],
	   [alternateOf(e_2,e_1)]))).
	 
decl('Inference', 13, 'attribution-inference',
     rule([att, e, ag, attrs],
	  [wasAttributedTo(att, e,ag,attrs)],
	  [a, t, gen, assoc, pl],
	  [wasGeneratedBy(gen, e,a,t,[]),
	   wasAssociatedWith(assoc, a,ag,pl,[])])).

decl('Inference', 14, 'delegation-inference',
     rule( [id, ag_1, ag_2, a, attrs],
	   [actedOnBehalfOf(id, ag_1, ag_2, a, attrs)],
	   [ id_1, pl_1, id_2, pl_2 ],
	   [wasAssociatedWith(id_1, a, ag_1, pl_1, []),
	    wasAssociatedWith(id_2, a, ag_2, pl_2, [])])).
  
decl('Inference', 15, 'influence-inference',
     rules([rule( [id, e, a, t, attrs],
		  [wasGeneratedBy(id, e,a,t,attrs)],
		  [wasInfluencedBy(id, e, a, attrs)]),
	    rule( [id, a, e, t, attrs],
		  [used(id, a,e,t,attrs)],
		  [wasInfluencedBy(id, a, e, attrs)]),
	    rule( [id, a_2, a_1, attrs],
		  [wasInformedBy(id, a_2,a_1,attrs)],
		  [wasInfluencedBy(id, a_2, a_1, attrs)]),
	    rule( [id, a_2, e, a_1, t, attrs],
		  [wasStartedBy(id, a_2,e,a_1,t,attrs)],
		  [wasInfluencedBy(id, a_2, e, attrs)]),
	    rule( [id, a_2, e, a_1, t, attrs],
		  [wasEndedBy(id, a_2,e,a_1,t,attrs)],
		  [wasInfluencedBy(id, a_2, e, attrs)]),
	    rule( [id, e, a, t, attrs],
		  [wasInvalidatedBy(id, e,a,t,attrs)],
		  [wasInfluencedBy(id, e, a, attrs)]),
	    rule( [id, e_2, e_1, a, g, u, attrs],
		  [wasDerivedFrom(id, e_2, e_1, a, g, u, attrs)],
		  [wasInfluencedBy(id, e_2, e_1, attrs)]),
	    comment(['In this rule, ',
		    math(a), ',', 
		    math(g), ', or ', 
		    math(u),' may be placeholders -.'],
	    rule( [id, e, ag, attrs],
		  [wasAttributedTo(id, e,ag,attrs)],
		  [wasInfluencedBy(id, e, ag, attrs)])),
	    comment(['In this rule, ',math(pl),' may be a placeholder -.'],
	    rule( [id, a, ag, pl, attrs],
		  [wasAssociatedWith(id, a,ag,pl,attrs)],
		  [wasInfluencedBy(id, a, ag, attrs)])),
	    rule( [id, ag_2, ag_1, a, attrs],
		  [actedOnBehalfOf(id, ag_2,ag_1,a,attrs)],
		  [wasInfluencedBy(id, ag_2, ag_1, attrs)])])).

decl('Inference', 16, 'alternate-reflexive',
     rule([e],
	  [entity(e)],
	  [],
	  [alternateOf(e,e)])).

decl('Inference', 17, 'alternate-transitive',
     rule([e_1, e_2, e_3],
	  [alternateOf(e_1,e_2), alternateOf(e_2,e_3)],
	  [],
	  [alternateOf(e_1,e_3)])).

decl('Inference', 18, 'alternate-symmetric',
     rule([e_1, e_2],
	  [alternateOf(e_1,e_2)],
	  [],
	  [alternateOf(e_2,e_1)])).

decl('Inference', 19, 'specialization-transitive',
     rule([e_1, e_2, e_3 ],
	  [specializationOf(e_1,e_2), specializationOf(e_2,e_3)],
	  [],
	  [specializationOf(e_1,e_3)])).

decl('Inference', 20, 'specialization-alternate-inference',
     rule([e_1, e_2],
      [specializationOf(e_1,e_2)],
      [],
      [alternateOf(e_1,e_2)])).

decl('Inference', 21, 'specialization-attributes-inference',
     rule([e_1, attrs, e_2],
	  [entity(e_1, attrs), specializationOf(e_2,e_1)],
	  [],
	  [entity(e_2, attrs)])).

%% Constraints.  Most of these remain to do.

decl('Constraint', 22, 'key-object',
     rules([key(id,entity(id,attrs)),
	    key(id,activity(id,t1,t2,attrs)),
	    key(id,agent(id,attrs))])).

decl('Constraint', 23, 'key-properties',
     rules([key(id,wasGeneratedBy(id, e,a,t,attrs) ),
	    key(id,used(id, a,e,t,attrs)),
	    key(id,wasInformedBy(id, a_2,a_1,attrs) ),
	    key(id,wasStartedBy(id, a_2,e,a_1,t,attrs) ),
	    key(id,wasEndedBy(id, a_2,e,a_1,t,attrs) ),
	    key(id,wasInvalidatedBy(id, e,a,t,attrs) ),
	    key(id,wasDerivedFrom(id, e_2, e_1, a, g2, u1, attrs) ),
	    key(id,wasAttributedTo(id, e,ag,attr) ),
	    key(id,wasAssociatedWith(id, a,ag,pl,attrs) ),
	    key(id,actedOnBehalfOf(id, ag_2,ag_1,a,attrs) ),
	    key(id,wasInfluencedBy(id, o2,o1,attrs) ) ])).

decl('Constraint',24,'unique-generation',
     rule([gen_1,gen_2,e,a,t_1,t_2,attrs_1,attrs_2],
	  [wasGeneratedBy(gen_1, e,a,t_1,attrs_1),
	   wasGeneratedBy(gen_2, e,a,t_2,attrs_2)],
	  [], [gen_1 = gen_2])).

decl('Constraint',25,'unique-invalidation',
     rule([inv_1,inv_2,e,a,t_1,t_2,attrs_1,attrs_2],
	  [wasInvalidatedBy(inv_1, e,a,t_1,attrs_1),
	   wasInvalidatedBy(inv_2, e,a,t_2,attrs_2)],
	  [], [inv_1 = inv_2])).

decl('Constraint', 26,'unique-wasStartedBy',
     rule( [ start_1,start_2,a,e_1,e_2,a_0,t_1,t_2,attrs_1,attrs_2 ], 
	   [wasStartedBy(start_1, a,e_1,a_0,t_1,attrs_1)  ,  
	    wasStartedBy(start_2, a,e_2,a_0,t_2,attrs_2) ],
	   [],
	   [ start_1 = start_2])).

decl('Constraint', 27 ,'unique-wasEndedBy',
     rule([ end_1,end_2,a,e_1,e_2,a_0,t_1,t_2,attrs_1,attrs_2 ], 
	  [wasEndedBy(end_1, a,e_1,a_0,t_1,attrs_1)  ,  
	   wasEndedBy(end_2, a,e_2,a_0,t_2,attrs_2)],
	  [],
	  [ end_1 = end_2])).
  
decl('Constraint', 28, 'unique-startTime',
     rule( [ start,a_1,a_2,t,t_1,t_2,e,attrs,attrs_1 ], 
	   [activity(a_2,t_1,t_2,attrs)  ,  
	    wasStartedBy(start, a_2,e,a_1,t,attrs_1) ],
	   [],[ t_1=t])).


decl('Constraint', 29, 'unique-endTime',
     rule( [  end,a_1,a_2,t,t_1,t_2,e,attrs,attrs_1], 
	   [activity(a_2,t_1,t_2,attrs)  ,  
	    wasEndedBy(end, a_2,e,a_1,t,attrs_1) ],
	   [],
	   [ t_2 = t])).

decl('Constraint', 30, 'start-precedes-end',
     rule( [  start,end,a,e_1,e_2,a_1,a_2,t_1,t_2,attrs_1,attrs_2], 
	   [wasStartedBy(start, a,e_1,a_1,t_1,attrs_1),
	    wasEndedBy(end, a,e_2,a_2,t_2,attrs_2) ],
	   [],
	   [ preceq(start,end)])).

decl('Constraint', 31, 'start-start-ordering',
     rule( [ start_1,start_2,a,e_1,e_2,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
	   [ wasStartedBy(start_1, a,e_1,a_1,t_1,attrs_1)  ,  
	     wasStartedBy(start_2, a,e_2,a_2,t_2,attrs_2) ], 
	   [],
	   [ preceq(start_1,  start_2)])).

decl('Constraint', 32, 'end-end-ordering',
     rule( [ end_1,end_2,a,e_1,e_2,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
	   [ wasEndedBy(end_1, a,e_1,a_1,t_1,attrs_1)  ,  
	     wasEndedBy(end_2, a,e_2,a_2,t_2,attrs_2) ], 
	   [],
	   [ preceq(end_1,  end2)])).

decl('Constraint', 33, 'usage-within-activity',
     rules([rule( 
	    [ start,use,a,e_1,e_2,a_1,t_1,t_2,attrs_1,attrs_2 ], 
	   [ wasStartedBy(start, a,e_1,a_1,t_1,attrs_1)  ,  
	      used(use, a,e_2,t_2,attrs_2) ], 
	   [],
	   [ preceq(start,  use)]
	   ),
       rule( 
	    [ use,end,a,e_1,e_2,a_2,t_1,t_2,attrs_1,attrs_2 ], 
	   [ used(use, a,e_1,t_1,attrs_1)  ,  
	      wasEndedBy(end, a,e_2,a_2,t_2,attrs_2) ], 
	   [],
	   [ preceq(use,end)])])).

decl('Constraint', 34, 'generation-within-activity',
     rules([rule( 
		[ start,gen,e_1,e_2,a,a_1,t_1,t_2,attrs_1,attrs_2 ], 
	   [ wasStartedBy(start, a,e_1,a_1,t_1,attrs_1)  ,  
	      wasGeneratedBy(gen, e_2,a,t_2,attrs_2) ], 
	   [],
	   [ preceq(start,  gen)]),
	   rule( 
		[ gen,end,e,e_1,a,a_1,t,t_1,attrs,attrs_1 ], 
	   [ wasGeneratedBy(gen, e,a,t,attrs)  ,  
	      wasEndedBy(end, a,e_1,a_1,t_1,attrs_1) ], 
	   [],
	   [ preceq(gen,end)])])).

decl('Constraint', 35, 'wasInformedBy-ordering',
     rule( [ id,start,end,a_1,'a_1\'',a_2,'a_2\'',e_1,e_2,t_1,t_2,attrs,attrs_1,attrs_2 ], 
	   [ wasInformedBy(id, a_2,a_1,attrs)  ,  
	     wasStartedBy(start, a_1,e_1,'a_1\'',t_1,attrs_1)  ,  
	      wasEndedBy(end, a_2,e_2,'a_2\'',t_2,attrs_2) ], 
	   [],
	   [ preceq(start,  end)])).

decl('Constraint', 36, 'generation-precedes-invalidation',
     rule( [ gen,inv,e,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
	   [ wasGeneratedBy(gen, e,a_1,t_1,attrs_1)  ,  
	      wasInvalidatedBy(inv, e,a_2,t_2,attrs_2) ], 
	   [],
	   [ preceq(gen ,inv)])).

decl('Constraint', 37, 'generation-precedes-usage',
     rule( [ gen,use,e,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
	   [ wasGeneratedBy(gen, e,a_1,t_1,attrs_1)  ,  
	      used(use, a_2,e,t_2,attrs_2) ], 
	   [],
	   [ preceq(gen,  use)])).

decl('Constraint', 38, 'usage-precedes-invalidation',
     rule( [ use,inv,a_1,a_2,e,t_1,t_2,attrs_1,attrs_2 ], 
	   [ used(use, a_1,e,t_1,attrs_1)  ,  
	      wasInvalidatedBy(inv, e,a_2,t_2,attrs_2) ], 
	   [],
	   [ preceq(use,  inv)])).

decl('Constraint', 39, 'generation-generation-ordering',
     rule( [ gen_1,gen_2,e,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
	   [ wasGeneratedBy(gen_1, e,a_1,t_1,attrs_1)  ,  
	      wasGeneratedBy(gen_2, e,a_2,t_2,attrs_2) ], 
	   [],
	   [ preceq(gen_1,  gen_2)])).

decl('Constraint', 40, 'invalidation-invalidation-ordering',
     rule( [ inv_1,inv_2,e,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
	   [ wasInvalidatedBy(inv_1, e,a_1,t_1,attrs_1)  ,  
	      wasInvalidatedBy(inv_2, e,a_2,t_2,attrs_2) ], 
	   [],
	   [ preceq(inv_1,  inv_2)])).

decl('Constraint', 41, 'derivation-usage-generation-ordering',
     comment(['In this constraint', 
	      math(a), ',', 
	      math(gen_2), ', or ', 
	      math(use_1),' must not be placeholders -.'],
	     rule( [ d,e_1,e_2,a,gen_2,use_1,attrs ], 
		   [ notNull(a) ,  
		     notNull(gen_2) ,  
		     notNull(use_1) ,  
		     wasDerivedFrom(d, e_2,e_1,a,gen_2,use_1,attrs) ], 
		    [ preceq(use_1,  gen_2)]))).
  
decl('Constraint', 42, 'derivation-generation-generation-ordering',
     comment(['In this constraint, any of ',
	      math(a), ',', 
	      math(g), ', or ', 
	      math(u),' may be placeholders -.'],
	     rule( [ d,gen_1,gen_2,e_1,e_2,a,a_1,a_2,g,u,t_1,t_2,attrs,attrs_1,attrs_2 ], 
		   [ wasDerivedFrom(d, e_2,e_1,a,g,u,attrs)  ,  
		     wasGeneratedBy(gen_1, e_1,a_1,t_1,attrs_1)  ,  
		     wasGeneratedBy(gen_2, e_2,a_2,t_2,attrs_2) ], 
		   [ prec(gen_1, gen_2)]))).
  
decl('Constraint', 43, 'wasStartedBy-ordering',
     rules([rule([ gen,start,e,a,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
		 [ wasGeneratedBy(gen, e,a_1,t_1,attrs_1)  ,  
		   wasStartedBy(start, a,e,a_2,t_2,attrs_2) ], 
		 [ preceq(gen,  start)]),
	    rule([ start,inv,e,a,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
		 [ wasStartedBy(start, a,e,a_1,t_1,attrs_1)  ,  
		   wasInvalidatedBy(inv, e,a_2,t_2,attrs_2) ], 
		 [ preceq(start,  inv)])])).
  
decl('Constraint', 44, 'wasEndedBy-ordering',
     rules([rule([ gen,end,e,a,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
		 [ wasGeneratedBy(gen, e,a_1,t_1,attrs_1)  ,  
		   wasEndedBy(end, a,e,a_2,t_2,attrs_2) ], 
		 [ preceq(gen,   end)]),
	    rule([ end,inv,e,a,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
		 [ wasEndedBy(end, a,e,a_1,t_1,attrs_1)  ,  
		   wasInvalidatedBy(inv, e,a_2,t_2,attrs_2) ], 
		 [ preceq(end,  inv)])])).
  
decl('Constraint', 45, 'specialization-generation-ordering',
     rule( [ gen_1,gen_2,e_1,e_2,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
	   [ specializationOf(e_2,e_1)  ,  
	     wasGeneratedBy(gen_1, e_1,a_1,t_1,attrs_1)  ,  
	     wasGeneratedBy(gen_2, e_2,a_2,t_2,attrs_2) ], 
	   [ preceq(gen_1,  gen_2)])).
  
decl('Constraint', 46, 'specialization-invalidation-ordering',
     rule( [ inv_1,inv_2,e_1,e_2,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
	   [ specializationOf(e_1,e_2)  ,  
	     wasInvalidatedBy(inv_1, e_1,a_1,t_1,attrs_1)  ,  
	     wasInvalidatedBy(inv_2, e_2,a_2,t_2,attrs_2) ], 
	   [ preceq(inv_1,  inv_2)])).
  
decl('Constraint', 47, 'wasAssociatedWith-ordering',
     comment(['In the following inferences, ',
	     math(pl),' may be a placeholder -.'], 
	     rules([rule([ assoc,start_1,inv_2,ag,e_1,e_2,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
			 [ wasAssociatedWith(assoc, a,ag,pl,attrs)  ,  
			   wasStartedBy(start_1, a,e_1,a_1,t_1,attrs_1)  ,  
			   wasInvalidatedBy(inv_2, ag,a_2,t_2,attrs_2) ], 
			 [ preceq(start_1,  inv_2)]),
		    rule([ assoc,gen_1,end_2,ag,e_1,e_2,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
			 [ wasAssociatedWith(assoc, a,ag,pl,attrs)  ,  
			   wasGeneratedBy(gen_1, ag,a_1,t_1,attrs_1)  ,  
			   wasEndedBy(end_2, a,e_2,a_2,t_2,attrs_2) ], 
			 [ preceq(gen_1,  end_2)]),
		    rule([ assoc,start_1,end_2,ag,e_1,e_2,a_1,a_2,t_1,t_2,attrs_1,attrs_2], 
			 [ wasAssociatedWith(assoc, a,ag,pl,attrs)  ,  
			   wasStartedBy(start_1, a,e_1,a_1,t_1,attrs_1)  ,  
			   wasEndedBy(end_2, ag,e_2,a_2,t_2,attrs_2) ], 
			 [ preceq(start_1,  end_2)]),
		    rule([ assoc,start_1,end_2,ag,e_1,e_2,a_1,a_2,t_1,t_2,attrs_1,attrs_2 ], 
			 [ wasAssociatedWith(assoc, a,ag,pl,attrs)  ,  
			   wasStartedBy(start_1, ag,e_1,a_1,t_1,attrs_1)  ,  
			   wasEndedBy(end_2, a,e_2,a_2,t_2,attrs_2) ], 
			 [ preceq(start_1,  end_2)])]))).

decl('Constraint', 48, 'wasAttributedTo-ordering',
     rules([rule([ att,gen_1,gen_2,e,a_1,a_2,t_1,t_2,ag,attrs,attrs_1,attrs_2 ], 
		 [ wasAttributedTo(att, e,ag,attrs)  ,  
		   wasGeneratedBy(gen_1, ag,a_1,t_1,attrs_1)  ,  
		   wasGeneratedBy(gen_2, e,a_2,t_2,attrs_2) ], 
		 [ preceq(gen_1,  gen_2)]),
	    rule([ att,start_1,gen_2,e,e_1,a_1,a_2,ag,t_1,t_2,attrs,attrs_1,attrs_2 ], 
		 [ wasAttributedTo(att, e,ag,attrs)  ,  
		   wasStartedBy(start_1, ag,e_1,a_1,t_1,attrs_1)  ,  
		   wasGeneratedBy(gen_2, e,a_2,t_2,attrs_2) ], 
		 [ preceq(start_1,  gen_2)])])).

decl('Constraint', 49, 'actedOnBehalfOf-ordering',
     rules([rule([ del,gen_1,inv_2,ag_1,ag_2,a,a_1,a_2,t_1,t_2,attrs,attrs_1,attrs_2 ], 
		 [ actedOnBehalfOf(del, ag_2,ag_1,a,attrs)  ,  
		   wasGeneratedBy(gen_1, ag_1,a_1,t_1,attrs_1)  ,  
		   wasInvalidatedBy(inv_2, ag_2,a_2,t_2,attrs_2) ], 
		 [ preceq(gen_1,  inv_2)]),
	    rule([ del,start_1,end_2, ag_1,ag_2,a,a_1,a_2,e_1,e_2,t_1,t_2,attrs,attrs_1,attrs_2 ], 
		 [ actedOnBehalfOf(del, ag_2,ag_1,a,attrs)  ,  
		   wasStartedBy(start_1, ag_1,e_1,a_1,t_1,attrs_1)  ,  
		   wasEndedBy(end_2, ag_2,e_2,a_2,t_2,attrs_2) ], 
		 [ preceq(start_1,  end_2)])])).


decl('Constraint',51,'impossible-unspecified-derivation-generation-use',
     rules([rule([id,e_1,e_2,g,attrs],
		 [notNull(g),wasDerivedFrom(id,e_2,e_1,-,g,-,attrs)],
		 [],
		 ['False']),
	    rule([id,e_1,e_2,u,attrs],
		 [notNull(u),wasDerivedFrom(id,e_2,e_1,-,-,u,attrs)],
		 [],
		 ['False']),
	    rule([id,e_1,e_2,g,u,attrs],
		 [notNull(g),notNull(u),wasDerivedFrom(id,e_2,e_1,-,g,u,attrs)],
		 [],
		 ['False'])])).


decl('Constraint', 52, 'impossible-specialization-reflexive',
     rule([e],
	  [specializationOf(e,e)],
	  [],
	  ['False'])).

decl('Constraint', 53,'impossible-property-overlap',
     comment(['For each ',
	      math(r),'  and  ', 
	      math('s \\in \\{ used, wasGeneratedBy, wasInvalidatedBy, wasStartedBy, wasEndedBy, wasInformedBy, wasAttributedTo, wasAssociatedWith, actedOnBehalfOf\\}'),
	      ' such that ',
	      math(r),'  and  ',
	      math(s),
	      ' are different relation names, the following constraint holds:'],
	     rule([id,a_1,'\\ldots',a_m,b_1,'\\ldots',b_n],
		  [r(id,a_1,'\\ldots',a_m),s(id,b_1,'\\ldots',b_n)],
	  [],[ 'False']))).
     
decl('Constraint', 54,'impossible-object-property-overlap',
     comment(['For each ',
	      math('p \\in \\{entity,activity,agent\\}'),'  and each ', 
	      math('r \\in \\{ used, wasGeneratedBy, wasInvalidatedBy, wasStartedBy, wasEndedBy, wasInformedBy, wasAttributedTo, wasAssociatedWith, actedOnBehalfOf\\}'),
	      ', the following constraint holds:'],
	     rule([id,a_1,'\\ldots',a_m,b_1,'\\ldots',b_n],
		  [p(id,a_1,'\\ldots',a_m),r(id,b_1,'\\ldots',b_n)],
	  [],[ 'False']))).
     

decl('Constraint', 55,'entity-activity-disjoint',
     rule([id],
	  [typeOf(id,entity), typeOf(id,activity)],
	  [],[ 'False'])).

decl('Constraint',56, 'membership-empty-collection',
     rule([c,e],
	  [hasMember(c,e),typeOf(c,'prov:EmptyCollection')],
	  [],
	  ['False'])).




% rule(vars,hyps,vars,concl)

emit(X,Y,Z) :- atom(X),atom_concat(Y,X,Z).
emit(X,Y,Z) :- number(X),number_codes(X,Xc),atom_codes(X2,Xc),atom_concat(Y,X2,Z).

emitComment(A,_) --> {atom(A)}, emit(A).
emitComment([X|Xs],T) --> emitComments([X|Xs],T).
emitComment(math(A),latex) --> emit('$'),emit(A),emit('$').
emitComment(math(A),html) --> emit('<span class="math">'),emit(A),emit('</span>').
emitComment(math(A),wiki) --> emit('<math>'),emit(A),emit('</math>').

emitComments([],_) --> [].
emitComments([X|Xs],T) --> emitComment(X,T), emitComments(Xs,T).

emitListSep(_Sep,_Emit,[]) --> [].
emitListSep(_Sep,Emit,[P]) --> call(Emit,P).
emitListSep(Sep,Emit,[P,Q|Ps]) -->
	call(Emit,P),
	emit(Sep),
	emitListSep(Sep,Emit,[Q|Ps]).


latexPred(T = U) --> emit(T), emit(' = '), emit(U),!.
latexPred(typeOf(T,U)) --> emit(U), emit(' \\in typeOf('), emit(T), emit(')'),!.
latexPred(preceq(T,U)) --> emit(T), emit(' \\preceq '), emit(U),!.
latexPred(prec(T,U)) --> emit(T), emit(' \\prec '), emit(U),!.
latexPred(A) --> {atom(A)},emit(A).
latexPred(T) -->
	{T =.. [F|Ts], Ts \= []},
	emit(F),
	emit('('),
	latexPreds(Ts),
	emit(')').

latexPreds(Ps) --> emitListSep(',', latexPred, Ps).

latexConj(Ps) --> emitListSep(' \\wedge ', latexPred, Ps).

latexQuantify(_Q,[]) --> [].
latexQuantify(Q,Xs) --> {Xs \= []},
	emit(Q),
	emitListSep(',', emit, Xs),
	emit('.~').

latexRule(Xs,Hyps,Ys,Concls) -->
        latexQuantify('\\forall ',Xs),
	latexConj(Hyps),
	emit(' \\Rightarrow '),
	latexQuantify('\\exists ',Ys),
	latexConj(Concls).

latexInference(Xs,Hyps,Ys,Concls) -->
        emit('\\begin{array}[t]{l}\n'),
	latexQuantify('\\forall ',Xs),
	emit('\n\\\\\n\\qquad '),
	latexConj(Hyps),
	emit('\n\\\\\n\\quad\\Rightarrow\n'),
	latexQuantify('\\exists ',Ys),
	latexConj(Concls),
	emit('\n\\end{array}').
	
wikiRules(key(Id,Formula)) -->
  emit('The identifier field '),
  emit('<math>'),
  emit(Id),
  emit('</math>'),	
  emit(' is a KEY for the '),
  emit('<math>'),
  latexPred(Formula),
  emit('</math>'),	
  emit(' statement.').

wikiRules(rule(Xs,Hyps,Concls)) -->
	emit('<math>\n'),
% Change this to change from logical formula to inference rule format
	latexRule(Xs,Hyps,[],Concls),
	emit('\n</math>').

wikiRules(rule(Xs,Hyps,Ys,Concls)) -->
	emit('<math>\n'),
% Change this to change from logical formula to inference rule format
	latexRule(Xs,Hyps,Ys,Concls),
	emit('\n</math>').

wikiRules(comment(Comment,Rules)) -->
	emitComment(Comment,wiki),
	wikiRules(Rules).
wikiRules(rules(L)) -->
	wikiRuleList(L).

% only handles one nesting level
wikiRuleList([]) --> [].
wikiRuleList([R|Rs]) -->
	emit('\n#'),
	wikiRules(R),
	wikiRuleList(Rs).

wikiDecl(Type,Num,Name,Rules) -->
	emit('=== '),
	emit(Type),
	emit(' '),
	emit(Num),
	emit(' ('),
	emit(Name),
	emit(') ===\n'),
	wikiRules(Rules),
	emit('\n').



%% HTML stuff

%% Todo: Recognize subscripts and translate to <sub>

emitVar(A) --> {split('_',A,X,I), !},
	emit(X),
	emit('<sub>'),
	emit(I),
	emit('</sub>').
emitVar(A) --> emit(A).

split(C,A,A1,A2) :- atom(A),
	atom_chars(A,L),
	append(L1,[C|L2],L),
	atom_chars(A1,L1),
	atom_chars(A2,L2).

htmlPred(A) --> {atom(A)},
	emitVar(A).
htmlPred(T) -->
	{T =.. [F|Ts], Ts \= []},
	emit(F),
	emit('('),
	htmlPreds(Ts),
	emit(')').

htmlPreds(Ps) --> emitListSep(',', htmlPred, Ps).

htmlConj(Ps) --> emitListSep(' &#8743; ', htmlPred, Ps).

htmlQuantify(_Q,[]) --> [].
htmlQuantify(forall,Xs) --> {Xs \= []},
	emit('&#8704; '),
	emitListSep(',', emitVar, Xs),
	emit('. ').
htmlQuantify(exists,Xs) --> {Xs \= []},
	emit('&#8707; '),
	emitListSep(',', emitVar, Xs),
	emit('. ').

htmlRule(Xs,Hyps,Ys,Concls) -->
        htmlQuantify(forall,Xs),
	htmlConj(Hyps),
	emit(' &#10233; '),
	htmlQuantify(exists,Ys),
	htmlConj(Concls).

htmlRules(key(Id,Formula)) -->
  emit('The identifier field '),
  emit('<span class="math">'),
  emit(Id),
  emit('</span>'),	
  emit(' is a <span class="conditional">KEY</span> for the '),
  emit('<span class="math">'),
  htmlPred(Formula),
  emit('</span>'),	
  emit(' statement.').

htmlRules(rule(Xs,Hyps,Concls)) -->
	emit('<span class="math">'),
	htmlRule(Xs,Hyps,[],Concls),
	emit('</span>').

htmlRules(rule(Xs,Hyps,Ys,Concls)) -->
	emit('<span class="math">'),
	htmlRule(Xs,Hyps,Ys,Concls),
	emit('</span>').

htmlRules(comment(Comment,Rules)) -->
	emitComment(Comment,html),
	htmlRules(Rules).
htmlRules(rules(L)) -->
	emit('<ol>'),
	htmlRuleList(L),
	emit('</ol>').

% only handles one nesting level
htmlRuleList([]) --> [].
htmlRuleList([R|Rs]) -->
	emit('<li>'),
	htmlRules(R),
	emit('</li>'),
	htmlRuleList(Rs).

htmlDecl('Inference',_Num,Name,Rules) -->
	emit('<div class="inference" id="'),
	emit(Name),
	emit('">'),
	htmlRules(Rules),
	emit('</div>\n').
htmlDecl('Constraint',_Num,Name,Rules) -->
	emit('<div class="constraint" id="'),
	emit(Name),
	emit('">'),
	htmlRules(Rules),
	emit('</div>\n').


% html wrapping latex/MathJax

htmlLatexRules(key(Id,Formula)) -->
  emit('The identifier field '),
  emit('$'),
  emit(Id),
  emit('$'),	
  emit(' is a <span class="conditional">KEY</span> for the '),
  emit('$'),
  latexPred(Formula),
  emit('$'),	
  emit(' statement.').

htmlLatexRules(rule(Xs,Hyps,Concls)) -->
	emit('$'),
	latexInference(Xs,Hyps,[],Concls),
	emit('$').

htmlLatexRules(rule(Xs,Hyps,Ys,Concls)) -->
	emit('$'),
	latexInference(Xs,Hyps,Ys,Concls),
	emit('$').

htmlLatexRules(comment(Comment,Rules)) -->
	emitComment(Comment,latex),
        emit('<br />'),
	htmlLatexRules(Rules).
htmlLatexRules(rules(L)) -->
	emit('<ol>'),
	htmlLatexRuleList(L),
	emit('</ol>').

% only handles one nesting level
htmlLatexRuleList([]) --> [].
htmlLatexRuleList([R|Rs]) -->
	emit('<li>'),
	htmlLatexRules(R),
	emit('</li>'),
	htmlLatexRuleList(Rs).

htmlLatexDecl('Inference',Num,Name,Rules) -->
	emit('<div class="inference" number="'),
        emit(Num),
        emit('" id="'),
	emit(Name),
	emit('">'),
	htmlLatexRules(Rules),
	emit('</div>\n').
htmlLatexDecl('Constraint',Num,Name,Rules) -->
	emit('<div class="constraint" number="'),
        emit(Num),
        emit('" id="'),
	emit(Name),
	emit('">'),
	htmlLatexRules(Rules),
	emit('</div>\n').


writeEach(_Target,_Out,[]).
writeEach(Target,Out,[Num|Nums]) :-
	decl(Type,Num,Name,Rules),
	call(Target,Type,Num,Name,Rules,'',X),
	write(Out,X),
	flush_output(Out),
	writeEach(Target,Out,Nums).

		
main(Target,F) :- open(F,write,Out),
        findall(X, decl(_,X,_,_),Xs),
	writeEach(Target,Out,Xs),
	close(Out).

main :- findall(X, decl(_,X,_,_),Xs),
	writeEach(htmlLatexDecl,user,Xs).

