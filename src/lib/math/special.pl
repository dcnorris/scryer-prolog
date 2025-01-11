/** Special math functions in the Error, Gamma and Beta families

The underlying Rust implementations come from the puruspe crate.
*/

:- module(special, [
              erf/2
              ,erfc/2
              ,inverf/2
              ,inverfc/2
              ,gamma/2
              ,gammp/3
              ,gammq/3
              ,invgammp/3
              ,ln_gamma/2
                  %,beta/3
                  %,betai/4
                  %,invbetai/4
          ]).

%% erf(+X, -Erf)
%
% Erf is erf(X).
% TODO: This could be an opportunity to include formatted math via Djot;
% https://htmlpreview.github.io/?https://github.com/jgm/djot/blob/master/doc/syntax.html#math
% TODO: Include Djot references to NIST DLMF?  Or defer to puruspe's documentation?
erf(X, Erf) :-
    builtins:must_be_number(X, erf/2),
    '$erf'(X, Erf).

%% erfc(+X, -Erfc)
%
% Erfc is erfc(X).
erfc(X, Erfc) :-
    builtins:must_be_number(X, erfc/2),
    '$erfc'(X, Erfc).

%% inverf(+ErfX, -X)
%
% X is erf⁻¹(ErfX).
% ErfX ∈ (-1, 1)
inverf(ErfX, X) :-
    builtins:must_be_number(ErfX, inverf/2),
    '$inverf'(ErfX, X).

%% inverfc(+ErfcX, -X)
%
% X is erfc⁻¹(ErfcX).
% ErfcX ∈ (0, 2)
inverfc(ErfcX, X) :-
    builtins:must_be_number(ErfcX, inverfc/2),
    '$inverfc'(ErfcX, X).

%% gamma(+X, -Gamma)
%
% Gamma is Γ(X).
gamma(X, Gamma) :-
    builtins:must_be_number(X, gamma/2),
    '$gamma'(X, Gamma).

%% gammp(+A, +X, -P)
%
% P is γ(A,X)/Γ(A), the regularized _lower_ incomplete gamma function.
% X ≥ 0 is the upper limit of integration
% A > 0 is the shape parameter
gammp(A, X, P) :-
    builtins:must_be_number(A, gammp/3),
    builtins:must_be_number(X, gammp/3),
    '$gammp'(A, X, P).

%% gammq(+A, +X, -Q)
%
% Q is Γ(A,X)/Γ(A), the regularized _upper_ incomplete gamma function.
% X ≥ 0 is the lower limit of integration
% A > 0 is the shape parameter
gammq(A, X, Q) :-
    builtins:must_be_number(A, gammq/3),
    builtins:must_be_number(X, gammq/3),
    '$gammq'(A, X, Q).

%% invgammp(+P, +A, -X)
%
% X is the unique solution of P = P(A,X), where P(-,-) is the
% regularized lower incomplete gamma function.
% P ∈ [0,1] is a probability
% A > 0 is the _shape parameter_
invgammp(P, A, -X) :-
    builtins:must_be_number(P, invgammp/3),
    builtins:must_be_number(A, invgammp/3),
    '$invgammp'(P, A, X).

/* TODO: Investigate this..  Is there some issue of principal
         values to consider?
?- A = 3, X = 1.2, gammp(A, X, P), invgammp(P, A, Xb).
   A = 3, X = 1.2, P = 0.12051290121804392, Xb = - (1.2).
*/

%% ln_gamma(+X, -LnGamma)
%
% LnGamma is ln(Γ(X)), the natural logarithm of Γ(X).
ln_gamma(X, LnGamma) :-
    builtins:must_be_number(X, ln_gamma/2),
    '$ln_gamma'(X, LnGamma).

%% TODO: beta(+Z, +W, -B)
%
% B is B(Z,W) := Γ(Z)*Γ(W)/Γ(Z+W)

%% TODO: betai(+A, +B, +X, -I)
%
% I is Iₓ(A,B) := B(X;A,B)/B(A,B), the regularized incomplete beta function
% A > 0 and B > 0 are the first and second shape parameters
% X ∈ [0,1] is the upper limit of integration.

%% TODO: invbetai(+P, +A, +B, -X)
%
% X is the unique solution of P = Iₓ(A,B) := B(X;A,B)/B(A,B)
% P ∈ [0,1] is a probability
% A > 0 and B > 0 are the first and second shape parameters
% X ∈ [0,1] is the upper limit of integration.
