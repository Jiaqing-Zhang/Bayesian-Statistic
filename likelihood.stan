functions { /* saved as likelihood.stan in R's working directory */
  vector likelihood (vector chi, real e_j, real c_j) {
    int K=rows(chi);
    vector[K] part1= 1.414214 * (2* chi - 2 * c_j) ;
    vector[K] part2=Phi(part1);
    vector[K] part3=2 * part2 - 1;
    
    vector[K] like = ((1 - 2 * e_j) * part3 + 1)/2;
    return like;
  }
}










  