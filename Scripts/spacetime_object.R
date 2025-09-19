library(tidyverse)
library(sf)
library(spdep)
library(sfdep)
library(purrr)

#Objects from the IIP-DB (2024)----
## inventor: original inventor table from the IIP-DB / this incorporates the inventor's geocoded information
## ap: patent data from the IIP-DB / this incorporates the IPC_class.csv
## mun_sf: municipalities shapefile used for the analysis
## This "japan_list" object is a list of the different categories by period
## The object "japan" is saved as a csv file in "Data/japan_data.csv"

japan <- inventor |>
  inner_join(
    ap,
    by = "ida"
  ) |>
  filter(NBER != "6") |>
  left_join(pat_n, by = "ida") |>
  mutate(
    n = 1 / n_pat,
    period = cut(year, 40, labels = FALSE, ordered_result = TRUE),
    t_frame = case_when(
      period >= 1 & period <= 20 ~ "first",
      period >= 20 & period <= 40 ~ "second"
    )
  ) |>
  group_by(NBER_name, period, t_frame, JCODE) |>
  summarise(
    n_patents = sum(n),
    .groups = "drop"
  )

write_csv(japan, "Data/japan_data.csv")

japan_list <- split(japan, japan$NBER_name)

#Function to convert the list into a spacetime object (sfdep)----

space_time_object <- function(x) {
  tech_name <- unique(x$NBER_name)

  times <- unique(x$t_frame)

  spacetime_result <- list()

  for (time in times) {
    x_filter_period <- x |>
      filter(t_frame == time) |>
      group_by(JCODE) |>
      summarize(n_zero = n()) |>
      ungroup() |>
      filter(n_zero == 20)

    x_filter <- x |>
      filter(JCODE %in% x_filter_period$JCODE & t_frame == time)

    df <- mun_sf |>
      filter(JCODE %in% x_filter$JCODE) |>
      expand(JCODE, period = c(min(x_filter$period):max(x_filter$period))) |>
      left_join(
        x_filter,
        by = c("JCODE", "period")
      ) |>
      mutate(NBER_name = if_else(is.na(NBER_name), tech_name, NBER_name)) |>
      arrange(desc(JCODE)) |>
      left_join(japan_tot, by = "JCODE") |>
      mutate(
        n_lq = (n_patents / cat_patents) /
          (P_NUM / sum(mun_sf$P_NUM, na.rm = TRUE))
      )

    geo <- mun_sf |>
      filter(JCODE %in% df$JCODE)

    df_spacetime <- spacetime(
      .data = df,
      .geometry = geo,
      .loc_col = "JCODE",
      .time_col = "period"
    )

    spacetime_result[[time]] <- df_spacetime
  }

  return(spacetime_result)
}

#Run the function: output is a list of spacetime cubes "japan_sptime"

japan_sptime <- map(japan_list, .f = space_time_object)
