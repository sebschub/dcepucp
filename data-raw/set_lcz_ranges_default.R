lcz_ranges <- list(
  "Compact high-rise" = list(
    h2w = list(min = 2, max = NA),
    fr_build = list(min = 0.40, max = 0.60),
    fr_street = list(min = 0.40, max = 0.60),
    height = list(min = 25, max = NA)
  ),
  "Compact midrise" =  list(
    h2w = list(min = 0.75, max = 2),
    fr_build = list(min = 0.40, max = 0.70),
    fr_street = list(min = 0.30, max = 0.50),
    height = list(min = 10, max = 25)
  ),
  "Compact low-rise" =  list(
    h2w = list(min = 0.75, max = 1.5),
    fr_build = list(min = 0.40, max = 0.70),
    fr_street = list(min = 0.20, max = 0.50),
    height = list(min = 3, max = 10)
  ),
  "Open high-rise" =  list(
    h2w = list(min = 0.75, max = 1.25),
    fr_build = list(min = 0.20, max = 0.40),
    fr_street = list(min = 0.30, max = 0.40),
    height = list(min = 25, max = NA)
  ),
  "Open midrise" =  list(
    h2w = list(min = 0.3, max = 0.75),
    fr_build = list(min = 0.20, max = 0.40),
    fr_street = list(min = 0.30, max = 0.50),
    height = list(min = 10, max = 25)
  ),
  "Open low-rise" =  list(
    h2w = list(min = 0.3, max = 0.75),
    fr_build = list(min = 0.20, max = 0.40),
    fr_street = list(min = 0.20, max = 0.50),
    height = list(min = 3, max = 10)
  ),
  "Leightweight low-rise" =  list(
    h2w = list(min = 1, max = 2),
    fr_build = list(min = 0.6, max = 0.90),
    fr_street = list(min = NA, max = 0.20),
    height = list(min = 2, max = 4)
  ),
  "Large low-rise" =  list(
    h2w = list(min = 0.1, max = 0.3),
    fr_build = list(min = 0.30, max = 0.50),
    fr_street = list(min = 0.40, max = 0.50),
    height = list(min = 3, max = 10)
  ),
  "Sparsely built" =  list(
    h2w = list(min = 0.1, max = 0.25),
    fr_build = list(min = 0.1, max = 0.20),
    fr_street = list(min = NA, max = 0.20),
    height = list(min = 3, max = 10)
  ),
  "Heavy industry" =  list(
    h2w = list(min = 0.2, max = 0.5),
    fr_build = list(min = 0.2, max = 0.30),
    fr_street = list(min = 0.2, max = 0.40),
    height = list(min = 5, max = 15)
  ))

lcz_default <- structure(list(
  "Compact high-rise" = list(
    h2w = 4,
    fr_build = 0.5,
    fr_street = 0.5,
    height = 40
  ),
  "Compact midrise" =  list(
    h2w = 1.5,
    fr_build = 0.55,
    fr_street = 0.4,
    height = 20
  ),
  "Compact low-rise" =  list(
    h2w = 1,
    fr_build = 0.50,
    fr_street = 0.30,
    height = 8
  ),
  "Open high-rise" =  list(
    h2w = 1.,
    fr_build = 0.30,
    fr_street = 0.35,
    height = 30
  ),
  "Open midrise" =  list(
    h2w = 0.5,
    fr_build = 0.30,
    fr_street = 0.40,
    height = 15
  ),
  "Open low-rise" =  list(
    h2w = 0.3,
    fr_build = 0.3,
    fr_street = 0.4,
    height = 8
  ),
  "Leightweight low-rise" =  list(
    h2w = 1.5,
    fr_build = 0.7,
    fr_street = 0.1,
    height = 3
  ),
  "Large low-rise" =  list(
    h2w = 0.2,
    fr_build = 0.4,
    fr_street = 0.45,
    height = 8
  ),
  "Sparsely built" =  list(
    h2w = 0.15,
    fr_build = 0.15,
    fr_street = 0.1,
    height = 8
  ),
  "Heavy industry" =  list(
    h2w = 0.3,
    fr_build = 0.25,
    fr_street = 0.30,
    height = 10
  )),
  class = "lcz"
)

devtools::use_data(lcz_default, overwrite = TRUE)
devtools::use_data(lcz_ranges, internal = TRUE, overwrite = TRUE)
