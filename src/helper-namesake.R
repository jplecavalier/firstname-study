fwrite(
  x = qc[, .(nb = sum(nb)), .(sex, name)][nb >= 300, {

    dist_matrix <- adist(name)
    pairs <- which((dist_matrix <= 2L) * upper.tri(dist_matrix) == 1L, arr.ind = TRUE)
    data.table(matrix(name[pairs], ncol = 2L))

  }, .(sex)],
  file = "namesake.csv"
)
