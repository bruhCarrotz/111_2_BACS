library(data.table)
library(lsa)

ac_bundles_dt <- fread(
        "G:/My Drive/111_2_BACS/HW9/piccollage_accounts_bundles.csv")
ac_bundles_matrix <- as.matrix(ac_bundles_dt[, -1, with=FALSE])

cos_sim <- cosine(ac_bundles_matrix)
cos_sim_add <- apply(cos_sim, 1, mean)
cos_sim_add_rank <- cos_sim_add[order(cos_sim_add, decreasing = TRUE)]

get_top5 <- function (bundle_name,data) {
  reg1 <- data[bundle_name,]
  reg2 <- reg1[order(reg1, decreasing = TRUE)]
  return (reg2[2:6])
}

get_top5("family", cos_sim)
