# simulate_data.R
#
# A script that simulates two scenarios for the CFU data in the BM110
# microbiology laboratory.
#
# We have two farms, A and B. We take 25 soil samples from each, sampling from
# a (log)normal distribution for the total CFU/mL per sample. Farm A does not
# use antibiotics, so only a small proportion of the sample (i.e. a proportion
# of the total population) is antibiotic-resistant, giving us a CFU/mL for the
# NA/Ampicillin plate. For farm B, a larger proportion of the population is
# antimicrobial-resistant.

# Scenario 1
#
# In scenario 1, the CFU/mL for farms A and B have the same underlying
# distribution.
dfm <- data.frame(farm=as.factor(c(rep("A", 25), rep("B", 25))),
                  NA_plate=c(rlnorm(25, meanlog=6*log(10), sdlog=1*log(10)),
                            rlnorm(25, meanlog=6*log(10), sdlog=1*log(10))),
                  amr_frac=c(rnorm(25, mean=0.05, sd=0.01),
                             rnorm(25, mean=0.3, sd=0.01))
                  ) |>
  dplyr::mutate(NA_Amp_plate = NA_plate * amr_frac) |>
  tidyr::pivot_longer(cols=c("NA_plate", "NA_Amp_plate"),
                      names_to="plate",
                      values_to="cfu") |>
  dplyr::mutate(farm_plate=interaction(farm, plate))
  

p1 <- ggplot2::ggplot(dfm, ggplot2::aes(x=farm_plate, y=cfu, colour=farm_plate)) +
  ggplot2::geom_boxplot(fill=NA) +
  ggplot2::geom_jitter(width=0.1) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(title="Distribution of total CFU/mL by farm",
                x="Farm/Plate", y="CFU/mL")
ggplot2::ggsave("farm_scenario_1.png", p1)

readr::write_delim(dfm |> dplyr::select(farm, plate, cfu),
                   file.path("..", "assets", "data", "farm_scenario_1.tsv"))

# Scenario 2
#
# In scenario 2, the CFU/mL for farms A and B have differing underlying
# distribution. Farm B has a notably lower CFU/mL overall.
dfm2 <- data.frame(farm=as.factor(c(rep("A", 25), rep("B", 25))),
                   NA_plate=c(rlnorm(25, meanlog=6*log(10), sdlog=1*log(10)),
                              rlnorm(25, meanlog=5*log(10), sdlog=1*log(10))),
                   amr_frac=c(rnorm(25, mean=0.05, sd=0.01),
                              rnorm(25, mean=0.3, sd=0.01))
) |>
  dplyr::mutate(NA_Amp_plate = NA_plate * amr_frac) |>
  tidyr::pivot_longer(cols=c("NA_plate", "NA_Amp_plate"),
                      names_to="plate",
                      values_to="cfu") |>
  dplyr::mutate(farm_plate=interaction(farm, plate))


p2 <- ggplot2::ggplot(dfm2, ggplot2::aes(x=farm_plate, y=cfu, colour=farm_plate)) +
  ggplot2::geom_boxplot(fill=NA) +
  ggplot2::geom_jitter(width=0.1) +
  ggplot2::scale_y_log10() +
  #ggplot2::facet_wrap(~plate) +
  ggplot2::labs(title="Distribution of total CFU/mL by farm",
                x="Farm/Plate", y="CFU/mL")
p2
ggplot2::ggsave("farm_scenario_2.png", p2)

readr::write_delim(dfm |> dplyr::select(farm, plate, cfu),
                   file.path("..", "assets", "data", "farm_scenario_2.tsv"))
