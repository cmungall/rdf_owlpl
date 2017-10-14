:- begin_tests(oio).

:- use_module(library(obo_metadata/oio)).

test(defined) :-
        findall(X-Y,has_related_synonym(X,Y),_).

    
:- end_tests(oio).


:- begin_tests(iao).

:- use_module(library(obo_metadata/iao_metadata)).

test(defined) :-
        findall(X-Y,definition(X,Y),_).

    
:- end_tests(iao).
    
:- begin_tests(all).

:- use_module(library(obo_metadata)).

test(defined) :-
        findall([X,Y,Z],synonym_scope(X,Y,Z),_).

    
:- end_tests(all).
    
