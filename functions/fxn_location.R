library(tidyverse)


# use herd id -------------------
fxn_add_location_event <- function(df) {
  df %>%
    mutate(
      location_event = HERDID
    )
}


# use pen number -----------------------------
fxn_assign_location_event_pen_template <- function(df) {
  df %>%
    mutate(pen_num = parse_number(PEN)) %>%
    mutate(
      location_event = case_when(
        (pen_num == 0) ~ "Unknown Location",
        (pen_num < 15) ~ "Location_1",
        (pen_num >= 41 & pen_num <= 50) ~ "Location_2",
        (pen_num >= 51 & pen_num <= 54) ~ "Location_3",
        (pen_num >= 75 & pen_num <= 87) ~ "Location_4",
        (pen_num >= 90 & pen_num <= 99) ~ "Location_5",
        (pen_num >= 700 & pen_num <= 765) ~ "Location_6",
        TRUE ~ "Unknown Location"
      )
    )
}


# location from parenell source file ------------------------------
fxn_assign_location_event_parnell_ANON <- function(df) {
  df %>%
    mutate(
      location_event = paste0("Herd ", str_sub(source_file_path, 18, 22))
    )
}

# location lesion------------------
fxn_detect_location_lesion <- function(df) {
  df %>%
    mutate(
      detectRR = case_when(
        str_detect(
          Remark,
          "RR|.RR|RR.|.RR.|RH|.RH|RH.|.RH.|ALL|.ALL|ALL.|.ALL."
        ) ~ "RR",
        TRUE ~ ""
      ),
      detectLR = case_when(
        str_detect(
          Remark,
          "LR|.LR|LR.|.LR.|LH|.LH|LH.|.LH.|ALL|.ALL|ALL.|.ALL."
        ) ~ "LR",
        TRUE ~ ""
      ),
      detectRF = case_when(
        str_detect(
          Remark,
          "RF|.RF|RF.|.RF.|BF|.BF|BF.|.BF.|ALL|.ALL|ALL.|.ALL."
        ) ~ "RF",
        TRUE ~ ""
      ),
      detectLF = case_when(
        str_detect(
          Remark,
          "LF|.LF|LF.|.LF.|BF|.BF|BF.|.BF.|ALL|.ALL|ALL.|.ALL."
        ) ~ "LF",
        TRUE ~ ""
      )
    ) %>%
    mutate(locate_lesion = paste0(detectRR, detectLR, detectRF, detectLF))
}


# location lesion------------------
fxn_detect_location_lesion_default <- function(df) {
  df %>%
    mutate(
      detectRR = case_when(
        str_detect(
          Remark,
          "RR|.RR|RR.|.RR.|RH|.RH|RH.|.RH.|ALL|.ALL|ALL.|.ALL."
        ) ~ "RR",
        TRUE ~ ""
      ),
      detectLR = case_when(
        str_detect(
          Remark,
          "LR|.LR|LR.|.LR.|LH|.LH|LH.|.LH.|ALL|.ALL|ALL.|.ALL."
        ) ~ "LR",
        TRUE ~ ""
      ),
      detectRF = case_when(
        str_detect(
          Remark,
          "RF|.RF|RF.|.RF.|BF|.BF|BF.|.BF.|ALL|.ALL|ALL.|.ALL."
        ) ~ "RF",
        TRUE ~ ""
      ),
      detectLF = case_when(
        str_detect(
          Remark,
          "LF|.LF|LF.|.LF.|BF|.BF|BF.|.BF.|ALL|.ALL|ALL.|.ALL."
        ) ~ "LF",
        TRUE ~ ""
      )
    ) %>%
    mutate(locate_lesion = paste0(detectRR, detectLR, detectRF, detectLF))
}
