#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector Bmsm_stage2_ll_cpp(const arma::mat& dat, const arma::mat& A, const arma::mat& gm, 
								const double& rhoe, const double& sigma1, const double& sigma2) {
	int T = dat.n_rows, k = A.n_cols;
	double ll = 0.0, ft = 0.0, pa = 1 / (2 * M_PI*sqrt(1 - rhoe*rhoe));
	arma::rowvec piA = trans(arma::ones<arma::vec>(k))/k;
	arma::rowvec z(k, arma::fill::zeros);
	arma::rowvec w(k, arma::fill::zeros);
	arma::rowvec d1(k, arma::fill::ones);
	arma::rowvec d2(k, arma::fill::ones);
	arma::rowvec C(k, arma::fill::zeros);
	arma::colvec LLs(T, arma::fill::zeros);
	arma::rowvec pia(k,arma::fill::zeros);
	piA = piA*A;
	pia.col(0) = 1;

	for (int t = 1; t<T+1; t++) {
		d1 = trans(dat(t - 1, 0)*arma::ones<arma::vec>(k));
		d2 = trans(dat(t - 1, 1)*arma::ones<arma::vec>(k));
		z = (d1 / (sigma1*gm.row(0))) % (d1 / (sigma1*gm.row(0))) +
			(d2 / (sigma2*gm.row(1))) % (d2 / (sigma2*gm.row(1))) -
			(d1 / (sigma1*gm.row(0))) % (d2 / (sigma2*gm.row(1))) * 2 * rhoe;
		w = pa*exp(-z / (2 * (1 - rhoe*rhoe))) / ((sigma1*gm.row(0)) % (sigma2*gm.row(1))) + 1e-16;

		C = w % piA;
		ft = accu(C);

		LLs(t - 1) = log(ft);

		if (ft == 0) {
			piA = pia*A;
		}
		else {
			piA = ((1 / ft)*C)*A;
		}

	}

	ll = -accu(LLs);
	return NumericVector::create(Named("LL") = ll);
}