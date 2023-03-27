#include <RcppArmadillo.h>
// using  namespace Rcpp;

template <class T> inline int sgn(T v) { return (v > T(0)) - (v < T(0)); }

inline int f(double x, double y, std::vector<double> s) 
{
  double v = (x - s[0]) * (s[3] - s[1]) - (y - s[1]) * (s[2] - s[0]);
  return sgn(v);
}

//' Is Two Segment Parallel?
//' 
//' Test whether two segments run parallel or across each other.
//' 
//' @param segment0 the four coordinates of the first segment, c(x0, y0, x1, y1)
//' @param segment1 the four coordinates of the second segment, c(x0, y0, x1, y1)  
//' 
//' @return Boolean TRUE or FALSE
//' @references https://math.stackexchange.com/questions/1342435/how-to-determine-if-2-line-segments-cross
//' @examples 
//' pos <- matrix(c(-5, -5, -5, 5), ncol=2)
//' segment0 <- c(pos[1,], pos[2,])
//' wall <- c(-5, 0, -2.5, 0)
//' is_cross(segment0, wall)
//' @export
// [[Rcpp::export]]
bool is_cross(std::vector<double> segment0, std::vector<double> segment1) 
{
   int test0 = f(segment0[0], segment0[1], segment1);
   int test1 = f(segment0[2], segment0[3], segment1);
   int test2 = f(segment1[0], segment1[1], segment0);
   int test3 = f(segment1[2], segment1[3], segment0);
   bool out  = (test0 != test1) && (test2 != test3) ? true : false;
   return out;
}

//' get_intersection_of_lines
//' 
//' get_intersection_of_lines
//'
//' @param line1_pointA self_pos
//' @param line1_pointB self_goal
//' @param line2_pointA other_pos
//' @param line2_pointB other_goal
//' 
//' @return a vector
//' @examples
//' pos  <- matrix(c(0, -5, 40, 0), ncol=2); pos
//' goal <- matrix(c(0, 5, -50, 0), ncol=2); goal
//' self_pos <- pos[,1]
//' self_goal <- goal[,1] 
//' other_pos <- pos[,2]
//' other_goal <- goal[,2] 
//' res <- get_intersection_of_lines(self_pos, self_goal, other_pos, other_goal)
//' ## [1] 0 0
//' @export
// [[Rcpp::export]]
arma::vec get_intersection_of_lines(arma::vec line1_pointA, // self_pos
                                   arma::vec line1_pointB, // self_goal
                                   arma::vec line2_pointA, // other_pos
                                   arma::vec line2_pointB) // other_goal
{
   // get vectors from points A to B
   arma::vec line1_vector = line1_pointB - line1_pointA; // self goal - position
   arma::vec line2_vector = line2_pointB - line2_pointA; // oth goal - position
   
   // get intermediate variables (just for ease of reading the expressions further below)
   double x_1 = line1_pointA[0]; // self_pos
   double y_1 = line1_pointA[1];
   double x_2 = line2_pointA[0];
   double y_2 = line2_pointA[1];
   double Deltax_1 = line1_vector[0];
   double Deltay_1 = line1_vector[1];
   double Deltax_2 = line2_vector[0];
   double Deltay_2 = line2_vector[1];
   
   // calculate how many of line1_vector is needed to reach the intersection
   arma::vec out;
   double denominator, t;
   
   denominator = Deltax_2 * Deltay_1 - Deltax_1 * Deltay_2;
   if (denominator == 0) { // the lines don't intersect
     out.set_size(1);       
     out.fill(NA_REAL);
     return out;
   } else {
     // get and return the intersection point
     t = ( Deltax_2 * (y_2 - y_1) - Deltay_2 * (x_2 - x_1) ) / denominator;
   }
   return line1_pointA + t*line1_vector;
 }

 
