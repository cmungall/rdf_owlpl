:- module(rdf_owl,
          [label_atom/2,
           term_labelify/2,

           owl_some/3,
           owl_all/3]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(rdf_owl/owl)).

:-op(300,xfy,some).
:-op(300,xfy,all).

label_atom(X,A) :-
        label(X,S^^_),
        atom_string(A,S).

term_labelify(V,V) :-
        var(V),
        !.
term_labelify([],[]) :- !.
term_labelify([H|T],[H2|T2]) :-
        !,
        term_labelify(H,H2),
        term_labelify(T,T2).
term_labelify(T,T2) :-
        T =.. [P|Args],
        Args=[_|_],
        !,
        term_labelify(Args,Args2),
        T2 =.. [P|Args2].

term_labelify(T,T-A) :-
        label_atom(T,A),
        !.
term_labelify(T,T).

%! owl_some(?Restr, ?Property, ?Obj) is nondet.
%
% true if Restr is an OWL expression SomeValuesFrom(Property,Obj)
owl_some(R,P,O) :-
        onProperty(R,P),
        someValuesFrom(R,O).

%! owl_all(?Restr, ?Property, ?Obj) is nondet.
%
% true if Restr is an OWL expression AllValuesFrom(Property,Obj)
owl_all(R,P,O) :-
        onProperty(R,P),
        allValuesFrom(R,O).

owl_restriction(R, some(P,O)) :-
        onProperty(R,P),
        someValuesFrom(R,O).

owl_restriction(R, all(P,O)) :-
        onProperty(R,P),
        someValuesFrom(R,O).



