/**/

:- module(testutils, [
              try_falsify/2
              ,real/1
              ,posreal/1
              ,interval/3
              ,call_free/2
              ,odd_t/3
              ,δ_inverses_t/5
              ,counterexample/1
          ]).

:- use_module(library(charsio)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(si)).

real(X) :-
    random(U),
    X is tan(pi*(U - 0.5)).

posreal(X) :-
    random(U),
    X is 1/U - 1.

interval(A, B, X) :-
    random(U),
    X is A + (B-A)*U.

:- meta_predicate(try_falsify(+, 1)).

% NOTE: NON-reproducible tests would in fact be MORE STRINGENT,
%       creating opportunities to detect rare error cases over time.
reproducibly :- set_random(seed(2025)).

try_falsify(N, G) :- reproducibly -> try_falsify_(N, G).
try_falsify_(N, G) :- N > 0,
                      (   call(G, false) -> counterexample(G)
                      ;   N_ is N - 1,
                          try_falsify_(N_, G)
                      ).

:- meta_predicate(odd_for(2, 0)).

odd_for(F, Any) :-
    call_free(Any, X),
    _X is -X,
    call(F, X, Fx),
    call(F, _X, _Fx),
    _Fx is -Fx.

call_free(G, V) :- term_variables(G, [V]), call(G).

:- meta_predicate(odd_t(2, 0, ?)).

odd_t(F, Any, T) :-
    (   call_free(Any, X),
        _X is -X,
        call(F, X, Fx),
        call(F, _X, _Fx),
        _Fx is -Fx -> T = true
    ;   T = false
    ).

:- meta_predicate(δ_inverses_t(?, 2, 2, 0, ?)).

δ_inverses_t(Δ, F, Finv, Any, T) :-
    (   call_free(Any, X),
        call(F, X, Fx),
        call(Finv, Fx, X_),
        (   abs(X - X_) < Δ -> T = true
        ;   T = false
        )
    ).

counterexample(G) :- format("% COUNTEREXAMPLE: ~w ~n~t", [G]).
