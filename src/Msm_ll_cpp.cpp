#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector Msm_ll_cpp(const arma::mat& pimat0, const arma::mat& omegat, const arma::mat& A) {
	int T = omegat.n_rows + 1, k = omegat.n_cols;
	double pidenom = 0.0, ll = 0.0;
	arma::mat piA(1, k);
	arma::mat pinum(1, k);
	arma::mat pmat(T, k);
	arma::colvec LLs(T - 1);

	LLs.zeros();
	piA.zeros();
	pinum.zeros();
	pmat.zeros();
	pmat.row(0) = pimat0;


	for (int i = 1; i<T; i++) {
		piA = pmat.row(i - 1)*A;
		pinum = omegat.row(i - 1) % piA;
		pidenom = arma::as_scalar(omegat.row(i - 1)*piA.t());

		if (pidenom == 0) {
			pmat(i, 0) = 1;
		}
		else {
			pmat.row(i) = pinum / pidenom;
		}

		LLs(i - 1) = log(pidenom);
	}

	ll = accu(LLs);
	ll = -1 * ll;
	return NumericVector::create(Named("LL") = ll);
}