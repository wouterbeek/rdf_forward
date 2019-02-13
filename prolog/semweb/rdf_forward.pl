:- module(
  rdf_forward,
  [
    rdf_forward/0
  ]
).

/** <module> RDF forward reasoner

@author Wotuer Beek
@version 2019
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(when)).

:- use_module(library(dcg)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(semweb/rdf_term)).

:- debug(entailments).

:- maplist(rdf_register_prefix, [rdf,rdfs]).

:- rdf_meta
   axiom(?, t),
   conclusion(t),
   rule(?, t, t).





%! rdf_forward is det.
%
% Run one single iteration of the forward chainer.
%
% Forward chaining is done when no new entailments are asserted.

rdf_forward :-
  forall(
    rule(Rule, Prems, Concl),
    rdf_forward(Rule, Prems, Concl)
  ).

rdf_forward(Rule, Prems, Concl) :-
  debug(entailments, "~w", [Rule]),
  forall(
    call(Prems),
    conclusion(Concl)
  ).

conclusion(tp(S,P,O)) :-
  tp(_, S, P, O), !.
conclusion(tp(S,P,O)) :-
  (   debugging(entailments)
  ->  flag(entailments, N, N+1),
      dcg_with_output_to(string(String), rdf_dcg_tp(S,P,O)),
      debug(entailments, "~D ~s", [N,String])
  ;   true
  ),
  assert_triple(mem(entailed), S, P, O).



axiom(rdf,  tp(rdf:type,           rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:subject,        rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:predicate,      rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:object,         rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:first,          rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:rest,           rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:value,          rdf:type,           rdf:'Property'  )).
axiom(rdf,  tp(rdf:nil,            rdf:type,           rdf:'List'      )).
axiom(rdf,  tp(P,                  rdf:type,           rdf:'Property'  )) :-
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).
axiom(rdfs, tp(rdf:type,           rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:domain,        rdfs:domain,        rdf:'Property'  )).
axiom(rdfs, tp(rdfs:range,         rdfs:domain,        rdf:'Property'  )).
axiom(rdfs, tp(rdfs:subPropertyOf, rdfs:domain,        rdf:'Property'  )).
axiom(rdfs, tp(rdfs:subClassOf,    rdfs:domain,        rdfs:'Class'    )).
axiom(rdfs, tp(rdf:subject,        rdfs:domain,        rdf:'Statement' )).
axiom(rdfs, tp(rdf:predicate,      rdfs:domain,        rdf:'Statement' )).
axiom(rdfs, tp(rdf:object,         rdfs:domain,        rdf:'Statement' )).
axiom(rdfs, tp(rdfs:member,        rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdf:first,          rdfs:domain,        rdf:'List'      )).
axiom(rdfs, tp(rdf:rest,           rdfs:domain,        rdf:'List'      )).
axiom(rdfs, tp(rdfs:seeAlso,       rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:isDefinedBy,   rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:comment,       rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:label,         rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdf:value,          rdfs:domain,        rdfs:'Resource' )).
axiom(rdfs, tp(rdf:type,           rdfs:range,         rdfs:'Class'    )).
axiom(rdfs, tp(rdfs:domain,        rdfs:range,         rdfs:'Class'    )).
axiom(rdfs, tp(rdfs:range,         rdfs:range,         rdfs:'Class'    )).
axiom(rdfs, tp(rdfs:subPropertyOf, rdfs:range,         rdf:'Property'  )).
axiom(rdfs, tp(rdfs:subClassOf,    rdfs:range,         rdfs:'Class'    )).
axiom(rdfs, tp(rdf:subject,        rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdf:predicate,      rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdf:object,         rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:member,        rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdf:first,          rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdf:rest,           rdfs:range,         rdf:'List'      )).
axiom(rdfs, tp(rdfs:seeAlso,       rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:isDefinedBy,   rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdfs:comment,       rdfs:range,         rdfs:'Literal'  )).
axiom(rdfs, tp(rdfs:label,         rdfs:range,         rdfs:'Literal'  )).
axiom(rdfs, tp(rdf:value,          rdfs:range,         rdfs:'Resource' )).
axiom(rdfs, tp(rdf:'Alt',          rdfs:subClassOf,    rdfs:'Container')).
axiom(rdfs, tp(rdf:'Bag',          rdfs:subClassOf,    rdfs:'Container')).
axiom(rdfs, tp(rdf:'Seq',          rdfs:subClassOf,    rdfs:'Container')).
axiom(rdfs, tp(rdfs:'ContainerMembershipProperty', rdfs:subClassOf, rdf:'Property')).
axiom(rdfs, tp(rdfs:isDefinedBy,   rdfs:subPropertyOf, rdfs:seeAlso    )).
axiom(rdfs, tp(rdfs:'Datatype',    rdfs:subClassOf,    rdfs:'Class'    )).
axiom(rdfs, tp(P,                  rdf:type,           rdfs:'ContainerMembershipProperty')) :-
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).
axiom(rdfs, tp(P,                  rdfs:domain,        rdfs:'Resource' )) :-
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).
axiom(rdfs, tp(P,                  rdfs:range,         rdfs:'Resource' )) :-
  when(ground(P), rdf_is_predicate(P)),
  rdf_container_membership_property(P).



rule(axiom(Profile), true, tp(S,P,O)) :-
  axiom(Profile, tp(S,P,O)).
rule(rdf('D2'), predicate(_,P), tp(P,rdf:type,rdf:'Property')).
rule(rdfs(2), (tp(_,P,rdfs:domain,C),tp(_,I,P,_)), tp(I,rdf:type,C)).
rule(rdfs(3), (tp(_,P,rdfs:range,C),tp(_,_,P,I)), tp(I,rdf:type,C)).
rule(rdfs('4a'), tp(_,X,_P,_), tp(X,rdf:type,rdfs:'Resource')).
rule(rdfs('4b'), tp(_,_,_P,X), tp(X,rdf:type,rdfs:'Resource')).
rule(rdfs(5), (tp(_,P,rdfs:subPropertyOf,Q),tp(_,Q,rdfs:subPropertyOf,R)), tp(P,rdfs:subPropertyOf,R)).
rule(rdfs(6), tp(_,P,rdf:type,rdf:'Property'), tp(P,rdfs:subPropertyOf,P)).
rule(rdfs(7), (tp(_,P,rdfs:subPropertyOf,Q),tp(_,S,P,O)), tp(S,Q,O)).
rule(rdfs(8), tp(_,C,rdf:type,rdfs:'Class'), tp(C,rdfs:subClassOf,rdfs:'Resource')).
rule(rdfs(9), (tp(_,C,rdfs:subClassOf,D),tp(_,I,rdf:type,C)), tp(I,rdf:type,D)).
rule(rdfs(10), tp(_,C,rdf:type,rdfs:'Class'), tp(C,rdfs:subClassOf,C)).
rule(rdfs(11), (tp(_,C,rdfs:subClassOf,D),tp(_,D,rdfs:subClassOf,E)), tp(C,rdfs:subClassOf,E)).
rule(rdfs(12), tp(_,P,rdf:type,rdfs:'ContainerMembershipProperty'), tp(P,rdfs:subPropertyOf,rdfs:member)).
rule(rdfs(13), tp(_,D,rdf:type,rdfs:'Datatype'), tp(D,rdfs:subClassOf,rdfs:'Literal')).
