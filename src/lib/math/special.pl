/** Special math functions in the Error, Gamma and Beta families

The underlying Rust implementations come from the puruspe crate.
*/

:- module(special, [
              erf/2
                  %,erfc/2
                  %,inverf/2
                  %,inverfc/2
                  %,gamma/2
                  %,gammq/3
                  %,invgammp/3
                  %,ln_gamma/2
                  %,beta/3
                  %,betai/4
                  %,invbetai/4
          ]).

%% erf(+X, -Erf)
%
% Erf is erf(X).
erf(X, Erf) :- '$erf'(X, Erf).

%% TODO: erfc(+X, -Erfc)
%
% Erfc is erfc(X).

%% TODO: inverf(+X, -InvErf)
%
% InvErf is erf⁻¹(X). % TODO: Check this; puruspe docs have a mistake.

%% TODO: inverfc(+X, -InvErfc)
%
% InvErfc is erfc⁻¹(X). % TODO: Check this; puruspe docs have a mistake.

%% TODO: gamma(+X, -Gamma)
%
% Gamma is Γ(X).

%% TODO: gammp(+A, +X, -P)
%
% P is P(A,X), the regularized _lower_ incomplete gamma function.
% X ≥ 0 is the upper limit of integration
% A > 0 is the shape parameter

%% TODO: gammq(+A, +X, -Q)
%
% Q is Q(A,X) := Γ(A,X)/Γ(A), the regularized _upper_ incomplete gamma
% function.
% X ≥ 0 is the lower limit of integration
% A > 0 is the shape parameter

%% TODO: invgammp(+P, +A, -X)
%
% X is the unique solution of P = P(A,X), where P(-,-) is the
% regularized lower incomplete gamma function.
% P ∈ [0,1] is a probability
% A > 0 is the _shape parameter_

%% TODO: ln_gamma(+Z, -LnGamma)
%
% LnGamma is ln(Γ(Z)), the natural logarithm of Γ(Z).

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
