# prepare PND150 Brain data

# Clear existing data and graphics
rm(list = ls())
graphics.off()

# load libraries
library(readr)
library(dplyr)
library(tibble)

# import data
mar_network <- paste0(
  "S:/MIND/IDDRC Cores/",
  "Core F_Biostatistics Bioinformatics and Research Design (BBRD)/",
  "VandeWater_MARAutism/Request_97/MAR4"
)

dataset <- readr::read_csv(
  file = paste0(mar_network, "/Data/AllPND150BrainData.csv"),
  col_types = readr::cols(
    Sample    = readr::col_character(),
    Section   = readr::col_character(),
    Dam       = readr::col_character(),
    Sex       = readr::col_character(),
    Treatment = readr::col_character(),
    Plate     = readr::col_character(),
    .default  = readr::col_double()
  )
)

# reduce to variables of interest / enforce column order
dataset <- dataset |>
  dplyr::select(
    Sample, Section, Dam, Sex, Treatment, Plate,
    G_CSF, Eotaxin, GM_CSF, IL_1a, Leptin, MIP_1a,
    IL_4, IL_1b, IL_2, IL_6, EGF, IL_13,
    IL_10, IL_12p70, IFNg, IL_5, IL_17a, IL_18,
    MCP_1, IP_10, GRO, VEGF, Fractalkine, LIX,
    MIP_2, TNFa, Rantes
  )

# variable labels (for downstream reporting)
labels <- list(
  Sample      = "Sample",
  Section     = "Brain Section",
  Dam         = "Dam",
  Sex         = "Sex",
  Treatment   = "MAR Group",
  Plate       = "Plate",
  G_CSF       = "G-CSF",
  Eotaxin     = "Eotaxin",
  GM_CSF      = "GM-CSF",
  IL_1a       = "IL-1a",
  Leptin      = "Leptin",
  MIP_1a      = "MIP-1a",
  IL_4        = "IL-4",
  IL_1b       = "IL-1b",
  IL_2        = "IL-2",
  IL_6        = "IL-6",
  EGF         = "EGF",
  IL_13       = "IL-13",
  IL_10       = "IL-10",
  IL_12p70    = "IL-12p70",
  IFNg        = "IFN-g",
  IL_5        = "IL-5",
  IL_17a      = "IL-17a",
  IL_18       = "IL-18",
  MCP_1       = "MCP-1",
  IP_10       = "IP-10",
  GRO         = "GRO",
  VEGF        = "VEGF",
  Fractalkine = "Fractalkine",
  LIX         = "LIX",
  MIP_2       = "MIP-2",
  TNFa        = "TNF-a",
  Rantes      = "Rantes"
)

brain <- tibble::tibble(dataset)
usethis::use_data(brain, overwrite = TRUE)
