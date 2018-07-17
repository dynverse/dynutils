#include <Rcpp.h>
using namespace Rcpp;

//' Project a set of points to to set of segments
//'
//' Finds the projection index for a matrix of points \code{x}, when
//' projected onto a set of segments defined by \code{segment_start} and \code{segment_end}.
//'
//' @param x a matrix of data points.
//' @param segment_start a matrix of segment start points.
//' @param segment_end a matrix of segment end points.
//'
//' @return A list with components
//'   \item{x_proj}{a matrix of projections of \code{x} onto the given segments.}
//'   \item{segment}{the index of the segment a point is projected on}
//'   \item{progression}{the progression of a projection along its segment}
//'   \item{distance}{the distance from each point in \code{x} to its projection in \code{x_proj}}
//'
//' @examples
//' x <- matrix(rnorm(50, 0, .5), ncol = 2)
//' segfrom <- matrix(c(0, 1, 0, -1, 1, 0, -1, 0), ncol = 2, byrow = TRUE)
//' segto <- segfrom / 10
//' fit <- project_to_segments(x, segfrom, segto)
//'
//' str(fit) # examine output
//'
//' @export
// [[Rcpp::export]]
List project_to_segments(NumericMatrix x, NumericMatrix segment_start, NumericMatrix segment_end) {

  // calculate lengths of the segments
  NumericVector length(segment_start.nrow());
  for (int i = 0; i < segment_start.nrow(); ++i) {
    length[i] = sum(pow(segment_end(i, _) - segment_start(i, _), 2.0));
  }

  // projections of x onto s
  NumericMatrix x_proj(x.nrow(), x.ncol());
  rownames(x_proj) = rownames(x);
  colnames(x_proj) = colnames(x);

  NumericVector distance(x.nrow());
  IntegerVector segment(x.nrow());
  NumericVector progression(x.nrow());

  // iterate over points in x
  for (int i = 0; i < x.nrow(); ++i) {
    NumericVector p = x(i, _);

    // store information on the closest segment
    NumericVector p_proj(x.ncol());
    int best_segment = -1;
    double best_progression = -1;
    double best_distance = R_PosInf;

    // iterate over the segments
    for (int segi = 0; segi < segment_start.nrow(); ++segi) {

      // project p orthogonally onto the segment
      NumericVector diff1 = segment_end(segi, _) - segment_start(segi, _);
      NumericVector diff2 = p - segment_start(segi, _);

      double test_progression = sum(diff1 * diff2) / length(segi);
      if (test_progression < 0) {
        test_progression = 0.0;
      }
      if (test_progression > 1) {
        test_progression = 1.0;
      }

      // calculate position of projection and the distance
      NumericVector test_p_proj = segment_start(segi, _) + test_progression * diff1;

      // calculate distance of projection and original point
      double test_distance = sum(pow(test_p_proj - p, 2.0));

      // if this is better than what was found earlier, store it
      if (test_distance < best_distance) {
        best_distance = test_distance;
        p_proj = test_p_proj;
        best_segment = segi;
        best_progression = test_progression;
      }
    }

    // save the best projection to the output data structures
    x_proj(i, _) = p_proj;
    distance[i] = best_distance;
    segment[i] = best_segment + 1; // increase by 1 for R
    progression[i] = best_progression;
  }

  distance.attr("names") = rownames(x);
  segment.attr("names") = rownames(x);
  progression.attr("names") = rownames(x);

  // return output
  List ret;
  ret["x_proj"] = x_proj;
  ret["distance"] = distance;
  ret["segment"] = segment;
  ret["progression"] = progression;

    return ret;
}
