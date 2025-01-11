/** Special math functions in the Error, Gamma and Beta families

The underlying Rust implementations come from the puruspe crate.
*/

:- module(special, [
              erf/2
              ,erfc/2
              ,inverf/2
              ,inverfc/2
              ,gamma/2
              ,gamma/3
              ,gamma_P_Q/4
              ,invgammp/3
              ,log_gamma/2
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
% Gamma is Γ(X), the ordinary gamma function.
gamma(X, Gamma) :-
    builtins:must_be_number(X, gamma/2),
    '$gamma'(X, Gamma).

%% gamma(+A, +X, -Gamma)
%
% Gamma is Γ(A,X), the _upper_ incomplete gamma function.
% A > 0 is the shape parameter
% X ≥ 0 is the lower limit of integration
gamma(A, X, Gamma) :-
    builtins:must_be_number(A, gammq/3),
    builtins:must_be_number(X, gammq/3),
    '$gammq'(A, X, Q),
    gamma(A, GammaA),
    Gamma is Q*GammaA.

%% gamma_P_Q(+A, +X, -P, -Q)
%
% P is γ(A,X)/Γ(X), the regularized _lower_ incomplete gamma function.
% Q is Γ(A,X)/Γ(X), the regularized _upper_ incomplete gamma function.
% A > 0 is the shape parameter
% X ≥ 0 is the lower limit of integration
gamma_P_Q(A, X, P, Q) :-
    builtins:must_be_number(A, gamma_P_Q/4),
    builtins:must_be_number(X, gamma_P_Q/4),
    '$gammp'(A, X, P),
    '$gammq'(A, X, Q).

%% invgammp(+A, +P, -X)
%
% X is the unique solution of P = P(A,X), where P(-,-) is the
% regularized lower incomplete gamma function.
% P ∈ [0,1] is a probability
% A > 0 is the _shape parameter_
invgammp(A, P, X) :-
    builtins:must_be_number(A, invgammp/3),
    builtins:must_be_number(P, invgammp/3),
    '$invgammp'(P, A, X).

%% log_gamma(+X, -LogGamma)
%
% LogGamma is ln(Γ(X)), the natural logarithm of Γ(X).
log_gamma(X, LnGamma) :-
    builtins:must_be_number(X, log_gamma/2),
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
