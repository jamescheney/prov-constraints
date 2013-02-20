
% Rudimentary import of PROV-XML
% stephen.cresswell@tso.co.uk
%
% Uses SWI-Prolog XML parser
%
% Issues:
%
% - Doesn't handle bundle
%
% - Using dialect(xmlns) option causes namespace expansion to
%    the form URI:LocalName. Namespace prefixes within attribute 
%    values and content are preserved are prefixes - but the
%    prefix mappings are not preserved in the output.
%

ns(prov,'http://www.w3.org/ns/prov#').

% Read XML from Filename and
% bind J to a list of Prolog terms like
%  [activity('ex:a1',_,_,[]), ... ]
% Assumes a single instance with no bundle tags
provx_load_instance(Filename,K):-
	load_structure(Filename,X,[dialect(xmlns),space(remove)]),
	provx(X,J),
	postprocess(J,K).

provx_load_document(Filename,K):-
	load_structure(Filename,X,[dialect(xmlns),space(remove)]),
        preprocess_document(X,D),
	provx_document(D,J),
	postprocess_document(J,K).

% Split document into toplevel XML and identified bundles 
preprocess_document([element(Prov:document,_,Contents)],(X,Bundles)):-
	ns(prov,Prov),
	preprocess_contents(Contents,X,Bundles).

preprocess_contents([],[],[]).
preprocess_contents([element(Prov:bundle,Attrs,X)|Xs],Toplevel,[(ID,X)|Bundles]) :-
  ns(prov,Prov),
  attribute_value(Prov:id,Attrs,ID),
  preprocess_contents(Xs,Toplevel,Bundles).

preprocess_contents([element(NS:Tag,Attrs,X)|Xs],
		    [element(NS:Tag,Attrs,X)|Toplevel], Bundles) :-
  Tag \= bundle, 
  preprocess_contents(Xs,Toplevel,Bundles).
		    

% Handle a document toplevel instance and bundles.
provx_document((X,Xs),(J,Bundles)) :-
  provx_expressions(X,J,[]),
  provx_bundles(Xs,Bundles).

provx_bundles([],[]).
provx_bundles([(ID,X)|Xs],
	      [(ID,J)|Bundles]) :-
  provx_expressions(X,J,[]), 
  provx_bundles(Xs,Bundles).

provx([element(Prov:document,_,Contents)|_Elements],J):-
	ns(prov,Prov),
	provx_expressions(Contents,J,[]).

provx_expressions([],Es,Es):-!.
provx_expressions([element(Prov:hadMember,_Attrs,[Collx|Ents])|Elements],Es0,Es2):-
	% hadMember is a special case, as it contains multiple members,
	% and each one is separate PROV-N statement
	ns(prov,Prov),
	Collx=element(Prov:collection,CollAttrx,_),
	attribute_value(Prov:ref,CollAttrx,CollID),
	provx_members(Ents,CollID,Es0,Es1),
	!,
	provx_expressions(Elements,Es1,Es2).
provx_expressions([element(Prov:Name,Attrs,Contents)|Elements],[Expression|Es0],Es1):-
	% template args start with id
	ns(prov,Prov),
	template(Name,[id|TArgs]),!,
	attribute_value(Prov:id,Attrs,ID),
	provx_args(TArgs,Contents,Args),
	Expression=..[Name,ID|Args],
	!,
	provx_expressions(Elements,Es0,Es1).
provx_expressions([element(Prov:Name,_Attrs,Contents)|Elements],[Expression|Es0],Es1):-
	% no id expected
	ns(prov,Prov),
	template(Name,TArgs),
	TArgs\=[id|_],
	provx_args(TArgs,Contents,Args),
	Expression=..[Name|Args],
	!,
	provx_expressions(Elements,Es0,Es1).
provx_expressions([E|_],_,_):-
	write('Failing on '),write(E),nl,
	fail.

provx_members([],_,Es,Es).
provx_members([element(Prov:entity,Attrs,Contents)|Elements],
	      Collection,
	      [hadMember(Collection,Entity)|Es0],
	      Es1):-
	!,
	ns(prov,Prov),
	arg_value(Attrs,Contents,Entity),
	provx_members(Elements,Collection,Es0,Es1).
provx_members(X,_,_,_):-
	write('Failing on collection'),write(X),nl.

provx_args([],_,[]).
provx_args([attrs],Elements,[Attrs]):-
	% Additional attributes
	!,
	provx_attributes(Elements,Attrs).
provx_args([Name|TArgs],[element(Prov:Name,Attrs,Contents)|Elements],[Value|Values]):-
	% obligatory argument
	ns(prov,Prov),
	!,
	arg_value(Attrs,Contents,Value),
	provx_args(TArgs,Elements,Values).
provx_args([+Name|TArgs],[element(Prov:Name,Attrs,Contents)|Elements],[Value|Values]):-
	% optional argument present
	ns(prov,Prov),
	!,
	arg_value(Attrs,Contents,Value),
	provx_args(TArgs,Elements,Values).
provx_args([+_|TArgs],Elements,[_|Args]):-
	% optional argument absent
	!,
	provx_args(TArgs,Elements,Args).
provx_args([-Name|TArgs],[element(Prov:Name,Attrs,Contents)|Elements],[Value|Values]):-
	% optional argument present
	ns(prov,Prov),
	!,
	arg_value(Attrs,Contents,Value),
	provx_args(TArgs,Elements,Values).
provx_args([-_|TArgs],Elements,[null|Args]):-
	% optional argument absent
	!,
	provx_args(TArgs,Elements,Args).
provx_args(TArgs,Elements,_):-
	write('Failing on args '),write(Elements),nl,
	write('Expected '),write(TArgs),nl,
	fail.

provx_attributes([],[]).
provx_attributes([element(Key,_Attrs,Contents)|Elements],[Key=Value|Attrs]):-
	% Expecting Contents single element list containing atom
	Contents=[Value|_], 
	provx_attributes(Elements,Attrs).

% If no Name=Val pair in Attrs, succeed anyway with Val unbound
attribute_value(Name,Attrs,Val):-
	memberchk(Name=Val,Attrs),
	!.
attribute_value(_,_,_).
	
arg_value(Attrs,_,ID):-
	% Find an ID
	ns(prov,Prov),
	memberchk(Prov:ref=ID,Attrs),
	!.
arg_value(_,[Content|_],Content).
	% Find content


% Arguments for each type of expression
% '+' prefix indicates optional argument
%   - we need a placeholder in Prolog term, but value might not be provided
template(entity,[id,attrs]).
template(activity,[id,+startTime,+endTime,attrs]).
template(agent,[id,attrs]).

template(wasGeneratedBy,[id,entity,+activity,+time,attrs]).
template(used,[id,activity,+entity,+time,attrs]).

template(wasInformedBy,[id,informed,informant,attrs]).
template(wasStartedBy,[id,activity,+trigger,+starter,+time,attrs]).
template(wasEndedBy,[id,activity,+trigger,+ender,+time,attrs]).
template(wasInvalidatedBy,[id,entity,+activity,+time,attrs]).

template(wasDerivedFrom,[id,generatedEntity,usedEntity,-activity,-generation,-usage,attrs]).
template(wasAttributedTo,[id,entity,agent,attrs]).
template(wasAssociatedWith,[id,activity,+agent,-plan,attrs]).
template(actedOnBehalfOf,[id,delegate,responsible,+activity,attrs]).
template(wasInfluencedBy,[id,influencee,influencer,attrs]).
template(specializationOf,[specificEntity,generalEntity]).
template(alternateOf,[alternate1,alternate2]).


expand(X,Y) :- (X == null -> Y = _; Y = X).

postprocess([],[]).
%postprocess([specializationOf(_,E1,E2,_)|J],
%	    [specializationOf(E1,E2)|K]) :-
%	!,postprocess(J,K).
%postprocess([alternateOf(_,E1,E2,_)|J],
%	    [alternateOf(E1,E2)|K]) :-
%	!,postprocess(J,K).
postprocess([wasDerivedFrom(ID,E2,E1,A,G1,U1,Attrs)|J],
	    [wasDerivedFrom(ID,E2,E1,A,G2,U2,Attrs)|K]) :-
	!,
	(A == null
	-> (G2 = G1, U2 = U1)
	; (expand(G1,G2),expand(U1,U2))),
	postprocess(J,K).
    
postprocess([A|J],[A|K]) :- postprocess(J,K).
	    
postprocess_document((I,Bundles),(J,Bundles2)) :-
  postprocess(I,J),
  postprocess_bundles(Bundles,Bundles2).

postprocess_bundles([],[]).
postprocess_bundles([(ID,I)|Bundles],[(ID,J)|Bundles2]) :-
  postprocess(I,J),
  postprocess_bundles(Bundles,Bundles2).
