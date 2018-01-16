:- module(rdf_owl,
          [regex_str/2,
           regex_str/3,
           label_atom/2,
           term_labelify/2,
           row_labelify/2,
           
           owl_some/3,
           owl_all/3]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(rdf_owl/owl)).
:- use_module(library(regex)).

:-op(300,xfy,some).
:-op(300,xfy,all).

regex_str(R,X) :-
        regex_str(R,X,'').
regex_str(R,X,Flag) :-
        X = S^^_,
        atom_string(S,A),
        A =~ R/Flag.

label_atom(X,A) :-
        \+ compound(X),
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

%! row_labelify(+Row,?LabeledRow) is det
%
% given a row term Foo(V1,V2,...,Vn)
% add an extra argument for the label for each
row_labelify(Row,Row2) :-
        Row =.. [P|Args],
        rowargs_labelify(Args,Args2),
        Row2 =.. [P|Args2].

rowargs_labelify([],[]).
rowargs_labelify([H|T],[H2,Label|T2]) :-
        (   label_atom(H,Label)
        ->  true
        ;   Label=''),
        compactify_arg(H,H2),
        rowargs_labelify(T,T2).

compactify_arg(H,H2) :-
        rdf_global_id(Curie,H),
        Curie\=H,
        !,
        sformat(H2,'~w',[Curie]).
compactify_arg(Str^^_,H2) :-
        atom_string(H2,Str),
        !.
compactify_arg(H,H).


        

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



