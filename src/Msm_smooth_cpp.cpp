#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix Msm_smooth_cpp(const arma::mat& A, const arma::mat& P) {
	int T = P.n_rows, k = P.n_cols;
	arma::mat p(T, k, arma::fill::zeros);
	arma::mat pt(T, k, arma::fill::zeros);
	

	pt.row(0) = trans(arma::ones<arma::vec>(k))/k;

	for (int t = 1; t < T; t++) {
		pt.row(t) = trans(A*trans(P.row(t - 1)));
	}

	p.row(T-1) = P.row(T-1);

	for (int t = T - 2; t >= 0; t--) {
		p.row(t) = 
			sum(
			kron(P.row(t), trans(p.row(t + 1))) % 
			(A/kron(trans(arma::ones<arma::vec>(k)), trans(pt.row(t + 1))))
		,0);
	}

	/*NumericMatrix p_smoothed = Rcpp::wrap(p);*/
	/*return NumericMatrix::create(Named("smoothedP") = p_smoothed);*/
	return(wrap(p));
}