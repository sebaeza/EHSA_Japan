library(purrr)
library(furrr)
library(sfdep)

# Run the emerging_hotspot_analysis (sfdep) in parallel for speed (furrr)----

plan("multisession", workers = 5)

ehsa_result <- future_map(
  japan_sptime,
  .f = ~ map(
    .x,
    ~ emerging_hotspot_analysis(
      x = .,
      .var = "n_patents",
      k = 1,
      nsim = 199,
      threshold = 0.05
    )
  ),
  .options = furrr_options(seed = c(1104063)),
  .progress = TRUE
)

plan("sequential")

# Writing the results files----

write_csv(
  map_dfr(
    ehsa_result,
    ~ map_dfr(.x, identity, .id = "period"),
    .id = "NBER"
  ),
  "Results/ehsa_results.csv"
)
