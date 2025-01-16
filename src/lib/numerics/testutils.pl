/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Written 2025 by David C. Norris (david@precisionmethods.guru)
   As with all things floating-point, use at your own risk.
   Part of Scryer Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/** Utility predicates for testing numerics

*/

:- module(testutils, [
              try_falsify/2
              ,real/1
              ,posreal/1
              ,interval/3
              ,call_free/2
              ,odd_t/3
              ,δ_inverses_t/5
          ]).

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(format)).

%% real(-X)
%
% X = tan(U) for uniform U ~ U[-π,π).
real(X) :-
    random(U),
    X is tan(pi*(U - 0.5)).

%% posreal(-X)
%
% X = 1/U for uniform U ~ U[0,1).
posreal(X) :-
    random(U),
    X is 1/U - 1.

%% interval(+A, +B, -X)
%
% X ~ U[A,B).
interval(A, B, X) :-
    random(U),
    X is A + (B-A)*U.

:- meta_predicate(try_falsify(+, 1)).

% NOTE: NON-reproducible tests would in fact be MORE STRINGENT,
%       creating opportunities to detect rare error cases over time.
reproducibly :- set_random(seed(2025)).

%% try_falsify(+N, ?G_1)
%
% Make N attempts to falsify the partial goal G/1, reporting the first
% counterexample found.
try_falsify(N, G_1) :- reproducibly -> try_falsify_(N, G_1).
try_falsify_(N, G_1) :- N > 0,
                      (   call(G_1, false) -> counterexample(G_1)
                      ;   N_ is N - 1,
                          try_falsify_(N_, G_1)
                      ).

%% call_free(?G, -V)
%
% Call goal G, the single free variable of which is bound to V.
call_free(G, V) :- term_variables(G, [V]), call(G).

:- meta_predicate(odd_t(2, 0, ?)).

%% odd_t(+F_2, +Any, ?T)
%
% T is the truth-value from testing that function F_2 is
% [odd](https://en.wikipedia.org/wiki/Even_and_odd_functions) at a
% value X obtained via call_free(Any, X).
odd_t(F, Any, T) :-
    (   call_free(Any, X),
        _X is -X,
        call(F, X, Fx),
        call(F, _X, _Fx),
        _Fx is -Fx -> T = true
    ;   T = false
    ).

:- meta_predicate(δ_inverses_t(?, 2, 2, 0, ?)).

%% δ_inverses_t(+Δ, +F_2, +Finv_2, +Any, ?T)
%
% For given functions F_2 and Finv_2, T is the truth-value from
% testing that Finv_2 ∘ F_2 is within Δ of the identity at a value X
% obtained via call_free(Any, X).
δ_inverses_t(Δ, F, Finv, Any, T) :-
    (   call_free(Any, X),
        call(F, X, Fx),
        call(Finv, Fx, X_),
        (   abs(X - X_) < Δ -> T = true
        ;   T = false
        )
    ).

counterexample(G) :- format("% COUNTEREXAMPLE: ~w ~n~t", [G]).
