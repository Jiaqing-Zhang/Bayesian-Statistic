functions { /* saved as likelihood.stan in R's working directory */
  vector likelihood (vector chi, real e_j, real c_j) {
  int K=rows (chi);
  real part1 = 1 - 2 * e_j;
  real part2 = 2 * Phi (sqrt(2) * (2 * chi - 2 * c_j)) - 1;
  real likelihood = ((part1 * part2) + 1 )/2;
  return (likelihood)
  }

  



