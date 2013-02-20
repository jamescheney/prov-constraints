% depends on checker and prov_xml
:- consult(checker).
:- consult(prov_xml).

test1(J):-
	provx_load_document('constraint_test_provx/unification-bundle-f1-FAIL-c55-c56.provx',J).

% Test and report on directory of .provx files
% - only tests whether the mapping completes, 
%  not whether result is correct,
test2(File,Files,J,PassOrFail):-
	Dir='constraint_test_provx',
	directory_files(Dir,Files),
	sort(Files,Files1),
	member(File,Files1),
	\+memberchk(File,['.','..']),
	absolute_file_name(File,AbsFile,[relative_to(Dir)]),
	exists_file(AbsFile),
	succeeds(provx_load_document(AbsFile,J),PassOrFail),
	write(PassOrFail),write('\t'),write(File),nl,nl.

test_all:-
	test2(_,_,_,_),
	fail.
test_all.

validate_all:-
	test2(File,_,J,pass),
	(valid_document(J)
	-> ( write('valid\t'),write(File), nl,nl)
	; (write('invalid\t'),write(File), nl,nl)),
	fail.
validate_all.

test3(File,J,K) :- Dir='constraint_test_provx',
	absolute_file_name(File,AbsFile,[relative_to(Dir)]),
	succeeds(provx_load_document(AbsFile,J),_),
	normalize_document(J,K).

succeeds(Goal,pass):-
	call(Goal),
	!.
succeeds(_Goal,fail).
