## Full refresh of all app data.

rm(list=ls())
library('magrittr')

competition_start_date <- as.Date("2022-01-25")

save_a_csv <- function(df, csv_name=deparse(substitute(df))){

  if(!grepl("\\.csv$", csv_name)){
    csv_name <- paste0(csv_name, ".csv")
  }

  readr::write_excel_csv(
    df,
    file.path(
      Sys.getenv("APP_BASE_PATH"),
      "duke_fintech_trading_competition_2022",
      csv_name,
      fsep = "\\"
    )
  )

}

read_a_csv <- function(csv_name){

  if(!grepl("\\.csv$", csv_name)){
    csv_name <- paste0(csv_name, ".csv")
  }

  suppressMessages(
    readr::read_csv(
      file.path(
        Sys.getenv("APP_BASE_PATH"),
        "duke_fintech_trading_competition_2022",
        csv_name,
        fsep = "\\"
      )
    )
  )

}

# ref_data #####################################################################
ref_data <- Sys.getenv("APP_BASE_PATH") %>%
  file.path(
    ., "duke_fintech_trading_competition_2022",  "benchmark.csv",
    fsep = "\\"
  ) %>%
  readr::read_csv(col_types = 'Dnn') %>% {
    dplyr::inner_join(
      .,
      Sys.getenv("APP_BASE_PATH") %>%
        file.path(
          ., "duke_fintech_trading_competition_2022",  "usdt_3mo_cmt.csv",
          fsep = "\\"
        ) %>%
        readr::read_csv(col_types = 'Dn'),
      by = "Date"
    )
  } %>%
  dplyr::transmute(
    "Date"        = Date,
    "SP500"       = round(.$SP500, 2),
    "SP500_rtn"   = c(NA, log(SP500[-1]/SP500[-length(SP500)])),
    "SHY"         = round(.$SHY, 2),
    "SHY_rtn"     = c(NA, log(SHY[-1]/SHY[-length(SHY)])),
    "BTC-USD"     = round(.$`BTC-USD`, 2),
    "BTC-USD_rtn" = c(NA, log(`BTC-USD`[-1]/`BTC-USD`[-length(`BTC-USD`)])),
    "3_mo"        = `3_mo`,
    "3_mo_apy"    = (1+.$`3_mo`/2)^2 - 1,
    "3_mo_td"     = `3_mo_apy`/252,
  ) %>%
  dplyr::filter(.$Date >= competition_start_date)
save_a_csv(ref_data)

recode <- function(df_col, key){
  vapply(
    df_col,
    function(x){
      key$correct_name[key$form_name == x]
    },
    character(1)
  )
}

# cleaned_wufoo_valid_registrants ##############################################
cleaned_wufoo_valid_registrants <- read_a_csv("wufoo_valid_registrants") %>%
  dplyr::mutate(
    'school'  = recode(school,  read_a_csv("school_recode_key")),
    'country' = recode(country, read_a_csv("country_recode_key"))
  )
save_a_csv(cleaned_wufoo_valid_registrants)

# deleted_accounts #############################################################
deleted_accounts <- Sys.getenv("APP_BASE_PATH") %>%
  file.path(
    "duke_fintech_trading_competition_2022", "deleted_accounts.txt", fsep = "\\"
  ) %>%
  readLines() %>%
  strsplit(", ") %>%
  unlist()

# flex_statement_df ############################################################
flex_statement_df <- Sys.getenv("APP_BASE_PATH") %>%
  file.path("..", "Downloads", fsep = "\\") %>%
  list.files(pattern = "^Mark-to-Market(.*).xml$", full.names = TRUE) %>%
  gsub("/", "\\\\", .) %>%
  stats::setNames(.,.) %>%
  vapply(
    function(flex_file_path){
      if(grepl("\\(\\d{1,}).xml", flex_file_path)){
        gsub("^(.*)\\(", "", flex_file_path) %>%
          gsub("\\)\\.xml$", "", .) %>%
          as.numeric()
      } else {
        0
      }
    },
    numeric(1)
  ) %>% {
    names(.[which.max(.)])
  } %>%
  xml2::read_xml() %>%
  xml2::xml_children() %>%
  xml2::xml_children() %>%
  lapply(
    function(FlexStatement){
      tibble::tibble(
        "account_id" = FlexStatement %>%
          xml2::xml_child("EquitySummaryInBase") %>%
          xml2::xml_child("EquitySummaryByReportDateInBase") %>%
          xml2::xml_attr("accountId"),
        "name_ibkr" = FlexStatement %>%
          xml2::xml_child("AccountInformation") %>%
          xml2::xml_attr("name"),
        "email_ibkr" = FlexStatement %>%
          xml2::xml_child("AccountInformation") %>%
          xml2::xml_attr("primaryEmail") %>%
          tolower(),
        "portfolio_value" = list(
          FlexStatement %>%
            xml2::xml_child("EquitySummaryInBase") %>%
            xml2::xml_children() %>%
            xml2::xml_attrs() %>%
            purrr::reduce(dplyr::bind_rows) %>%
            dplyr::transmute(
              "Date"        = as.Date(reportDate),
              "total_long"  = as.numeric(totalLong),
              "total_short" = as.numeric(totalShort),
              "total"       = as.numeric(total)
            ) %>%
            dplyr::filter(Date >= competition_start_date)
        ),
        "participating_S2022" = portfolio_value[[1]]$total %>%
          head(7) %>% {
            length(setdiff(., c(0, 1000000))) > 0
          }
      )
    }
  ) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::filter(!grepl("^DI", account_id))

# Chanel messed up her email. Need to use a 'key' that the students pick
#   instead of an email in the WuFoo form.
flex_statement_df$email_ibkr[
  flex_statement_df$name_ibkr == "Chanel Zhu Ms"
] <- "chanelchu@utexas.edu"
flex_statement_df$email_ibkr[
  flex_statement_df$name_ibkr == "Sreebhavana Pasumarthi"
] <- "pasumars@email.sc.edu"
flex_statement_df$email_ibkr[
  flex_statement_df$name_ibkr == "Yundi Zhang"
] <- "yundi.z@wustl.edu"
flex_statement_df$email_ibkr[
  flex_statement_df$name_ibkr == "Cheng Li"
] <- "cheng.li@wustl.edu"

# trader_key_private ###########################################################
trader_key_private <- flex_statement_df %>%
  dplyr::full_join(
    cleaned_wufoo_valid_registrants,
    by = c("email_ibkr" = "email")
  ) %>%
  dplyr::select(account_id, tradername, dplyr::everything()) %>%
  dplyr::mutate(
    "undergrad"       = graduate_dept %>%
      vapply(function(x){is.na(x)}, logical(1)),
    "status"          = purrr::pmap_chr(
      list(portfolio_value, account_id, DateCreated),
      function(pv, id, dc){
        if(id %in% deleted_accounts) return("deleted")
        if(is.na(id)) return("IBKR account not found")
        if(all(pv$total == 0)) return("limbo")
        if(
          any(pv$total == 1000000) &&
          length(setdiff(pv$total, c(0, 1000000))) == 0
        ){
          if(Sys.time() - dc > lubridate::month(1)){
            return("pending deletion")
          }
          return("inactive")
        }
        "active"
      }
    ),
    "first_trade_dt" = portfolio_value %>%
      vapply(
        function(pv){
          if(is.null(pv)) return(NA_character_)
          trade_idx <- which(pv$total != 0 & pv$total != 1000000)
          if(any(trade_idx)) return(as.character(pv$Date[min(trade_idx)]))
          NA_character_
        },
        character(1)
      ) %>%
      as.Date()
  ) %>%
  dplyr::select(
    account_id, tradername, status, first_trade_dt, dplyr::everything()
  ) %>%
  dplyr::arrange(tradername)
save_a_csv(trader_key_private)

# trader_key ###################################################################
trader_key <-  trader_key_private %>%
  dplyr::select(
    account_id, tradername, DateCreated, first_trade_dt, status,
    participating_S2022, country, school, name_ibkr, first_name, last_name,
    graduation_year, undergrad, undergrad_major, graduate_dept
  )

# account_value_df #############################################################
account_value_df <- trader_key_private %>%
  dplyr::filter(status == "active") %>%
  dplyr::select(tradername, portfolio_value)

# eod_account_value ############################################################
eod_account_value <- account_value_df$portfolio_value %>%
  lapply(
    function(x){x[,c("Date", "total")]}
  ) %>%
  purrr::reduce(dplyr::inner_join, by = "Date")  %>%
  dplyr::filter(Date %in% ref_data$Date) %>% {
    colnames(.) <- c("Date", account_value_df$tradername)
    . <- tibble::as_tibble(., .name_repair = "minimal")
    Encoding(colnames(.)) <- "UTF-8"
    .[. == 0] <- 1000000
    .
  }
save_a_csv(eod_account_value)

# eod_returns ##################################################################
eod_returns <- log(
  as.matrix(eod_account_value[-1,-1]) /
    as.matrix(eod_account_value[-nrow(eod_account_value),-1])
) %>% {
  colnames(.) <- account_value_df$tradername
  . <- tibble::as_tibble(., .name_repair = "minimal")
  Encoding(colnames(.)) <- "UTF-8"
  .
} %>%
  tibble::add_column(
    "Date"       = eod_account_value$Date[-1],
    .before      = TRUE,
    .name_repair = "minimal"
  )
save_a_csv(eod_returns)

# eod_excess_returns ###########################################################
rf <- ref_data$`3_mo_td`[match(eod_returns$Date, ref_data$Date)]/100
eod_excess_returns <- eod_returns %>%
  dplyr::mutate(
    dplyr::across(!Date, function(x){x - rf})
  )
save_a_csv(eod_excess_returns)

# cumulative_eod_excess_returns ################################################
eod_ex_returns_plus_1 <- eod_excess_returns %>%
  dplyr::mutate(
    dplyr::across(!Date, function(x){x + 1})
  )
cumulative_eod_excess_returns <- eod_ex_returns_plus_1$Date %>%
  lapply(
    function(eod_date){
      dplyr::filter(eod_ex_returns_plus_1, Date <= eod_date) %>%
        dplyr::select(-Date) %>%
        apply(
          MARGIN   = 2,
          FUN      = function(x){
            px <- prod(x)
            if(px < 0) return(mean(x))
            px^(1/length(x)) - 1
          }
        ) %>%
        tibble::as_tibble_row(.name_repair = "minimal") %>%
        tibble::add_column(
          "Date"       = eod_date,
          .before      = TRUE,
          .name_repair = "minimal"
        )
    }
  ) %>%
  purrr::reduce(dplyr::bind_rows) %>% {
    colnames(.) <- colnames(eod_ex_returns_plus_1)
    . <- tibble::as_tibble(., .name_repair = "minimal")
    Encoding(colnames(.)) <- "UTF-8"
    .
  }
save_a_csv(cumulative_eod_excess_returns)

# cumulative_vol ###############################################################
cumulative_vol <- eod_returns$Date[-1] %>%
  lapply(
    function(eod_date){
      dplyr::filter(eod_returns, Date <= eod_date) %>%
        dplyr::select(-Date) %>%
        apply(
          MARGIN   = 2,
          FUN      = function(x){
            vol <- sd(x)
            if(vol == 0) return(NA)
            vol
          }
        ) %>%
        tibble::as_tibble_row() %>%
        tibble::add_column(
          "Date"       = eod_date,
          .before      = TRUE,
          .name_repair = "minimal"
        )
    }
  ) %>%
  purrr::reduce(dplyr::bind_rows)
save_a_csv(cumulative_vol)

# sharpes ######################################################################
sharpes <- (
  as.matrix(cumulative_eod_excess_returns[-1,-1]) / as.matrix(
    cumulative_vol[,-1]
  )
) %>% {
  colnames(.) <- account_value_df$tradername
  . <- tibble::as_tibble(., .name_repair = "minimal") %>%
    tibble::add_column(
      "Date"       = cumulative_vol$Date,
      .before      = TRUE,
      .name_repair = "minimal"
    )
  Encoding(colnames(.)) <- "UTF-8"
  .
}
save_a_csv(sharpes)


# alphas & betas ###############################################################
ref_df <- ref_data %>%
  dplyr::select(Date, SP500_rtn, SHY_rtn, `BTC-USD_rtn`) %>%
  dplyr::inner_join(eod_returns, by = "Date")

alphas_SP500 <- alphas_SHY <- alphas_BTC <- betas_SP500 <- betas_SHY <-
  betas_BTC <- matrix(
    NA_integer_,
    nrow     = nrow(eod_returns) - 1,
    ncol     = ncol(eod_returns) - 1,
    dimnames = list(
      as.character(eod_returns$Date[-1]),
      colnames(eod_returns)[-1]
    )
  )

for(eod_date in rownames(alphas_SP500)){
  lil_ref_df <- ref_df %>%
    dplyr::filter(Date <= eod_date)
  for(trader in colnames(alphas_SP500)){
    response <- tibble::deframe(lil_ref_df[trader])
    lm_SP500 <- lm(response ~ lil_ref_df$SP500_rtn)
    lm_SHY   <- lm(response ~ lil_ref_df$SHY_rtn)
    lm_BTC   <- lm(response ~ lil_ref_df$`BTC-USD_rtn`)
    i <- which(rownames(alphas_SP500) == eod_date)
    j <- which(colnames(alphas_SP500) == trader)
    alphas_SP500[i,j] <- lm_SP500$coefficients[1]
    betas_SP500[i,j]  <- lm_SP500$coefficients[2]
    alphas_SHY[i,j]   <- lm_SHY$coefficients[1]
    betas_SHY[i,j]    <- lm_SHY$coefficients[2]
    alphas_BTC[i,j]   <- lm_BTC$coefficients[1]
    betas_BTC[i,j]    <- lm_BTC$coefficients[2]
  }
}

tibblify <- function(df){
  df %>%
    tibble::as_tibble(
      .name_repair = "minimal",
      rownames = "Date"
    ) %>% {
      Encoding(colnames(.)) <- "UTF-8"
      .
    } %>%
    dplyr::mutate(
      "Date" = as.Date(Date)
    )
}

alphas_SP500 <- tibblify(alphas_SP500)
alphas_SHY   <- tibblify(alphas_SHY)
alphas_BTC   <- tibblify(alphas_BTC)
betas_SP500  <- tibblify(betas_SP500)
betas_SHY    <- tibblify(betas_SHY)
betas_BTC    <- tibblify(betas_BTC)

save_a_csv(alphas_SP500)
save_a_csv(alphas_SHY)
save_a_csv(alphas_BTC)
save_a_csv(betas_SP500)
save_a_csv(betas_SHY)
save_a_csv(betas_BTC)

# standings ####################################################################
add_standings <- function(df, new_df, col_name){
  new_tbl <- tibble::tibble("tradername" = colnames(new_df)[-1])
  new_tbl[,col_name] <- as.numeric(
    new_df[which(new_df$Date == sharpes$Date[nrow(sharpes)]), -1]
  )
  dplyr::left_join(df, new_tbl, by = "tradername")
}
standings <- tibble::tibble(
  "tradername" = colnames(sharpes)[-1],
  "Sharpe"     = as.numeric(sharpes[nrow(sharpes),-1])
) %>%
  add_standings(cumulative_eod_excess_returns, "return") %>%
  add_standings(cumulative_vol, "volatility") %>%
  add_standings(alphas_SP500, "alpha_SP500") %>%
  add_standings(alphas_SHY, "alpha_SHY") %>%
  add_standings(alphas_BTC, "alpha_BTC") %>%
  add_standings(betas_SP500, "beta_SP500") %>%
  add_standings(betas_SHY, "beta_SHY") %>%
  add_standings(betas_BTC, "beta_BTC") %>%
  dplyr::left_join(
    dplyr::select(
      cleaned_wufoo_valid_registrants, tradername, school, country
    ),
    by = "tradername"
  ) %>%
  dplyr::select(tradername, school, country, dplyr::everything()) %>%
  dplyr::arrange(dplyr::desc(Sharpe)) %>%
  dplyr::filter(
    tradername %in% trader_key$tradername[which(trader_key$participating_S2022)]
  ) %>%
  tibble::add_column(
    "Rank"       = 1:nrow(.),
    .before      = TRUE,
    .name_repair = "minimal"
  )
save_a_csv(standings)

# trader_betas #################################################################
trader_betas <- trader_key %>%
  dplyr::filter(participating_S2022) %>%
  dplyr::select(tradername) %>%
  tibble::deframe() %>% {
    matrix(
      NA_real_,
      nrow = length(.),
      ncol = length(.),
      dimnames = list(., .)
    )
  }
for(j in 1:(ncol(trader_betas)-1)){
  for(i in (j+1):nrow(trader_betas)){
    trader_betas[i,j] <- lm(
      tibble::deframe(
        eod_returns[,which(colnames(eod_returns) == colnames(trader_betas)[j])]
      ) ~ tibble::deframe(
        eod_returns[,which(colnames(eod_returns) == rownames(trader_betas)[i])]
      )
    )$coefficients[2] %>%
      as.numeric()
  }
}

# last_updated #################################################################
last_updated <- Sys.getenv("APP_BASE_PATH") %>%
  file.path("duke_fintech_trading_competition_2022", fsep = "\\") %>%
  list.files(full.names = TRUE) %>%
  file.info() %>% {
    rownames(.) <- basename(rownames(.)) %>%
      gsub("\\.csv$", "", .)
    .
  } %>%
  tibble::as_tibble(rownames = "file") %>%
  dplyr::select(file, mtime) %>% {
    stats::setNames(.$mtime, .$file)
  } %>%
  as.list()

# save R data ##################################################################
usethis::use_data(ref_data,                      overwrite = TRUE)
usethis::use_data(trader_key,                    overwrite = TRUE)
usethis::use_data(eod_account_value,             overwrite = TRUE)
usethis::use_data(eod_returns,                   overwrite = TRUE)
usethis::use_data(eod_excess_returns,            overwrite = TRUE)
usethis::use_data(cumulative_eod_excess_returns, overwrite = TRUE)
usethis::use_data(cumulative_vol,                overwrite = TRUE)
usethis::use_data(alphas_SP500,                  overwrite = TRUE)
usethis::use_data(alphas_SHY,                    overwrite = TRUE)
usethis::use_data(alphas_BTC,                    overwrite = TRUE)
usethis::use_data(betas_SP500,                   overwrite = TRUE)
usethis::use_data(betas_SHY,                     overwrite = TRUE)
usethis::use_data(betas_BTC,                     overwrite = TRUE)
usethis::use_data(sharpes,                       overwrite = TRUE)
usethis::use_data(standings,                     overwrite = TRUE)
usethis::use_data(trader_betas,                  overwrite = TRUE)
usethis::use_data(last_updated,                  overwrite = TRUE)

# traderpages ##################################################################
traderpage_template <- file.path(".", "inst", "traderpage.Rmd") %>%
  readLines()
keepers             <- trader_key %>%
  dplyr::filter(
    status != "IBKR account not found" &
      status != "deleted" &
      !is.na(tradername)
  ) %>%
  dplyr::select(account_id) %>%
  unlist(use.names = FALSE) %>%
  unique()

# Write all the traderpages for account IDs in keepers
keepers %>%
  purrr::walk(
    function(account_id){
      traderpage_template %>%
        gsub("account_id_hook", account_id, .) %>%
        writeLines(
          file.path(".", "vignettes", "articles", paste0(account_id, ".Rmd"))
        )
    }
  )

# Delete any leftovers
Sys.getenv("APP_BASE_PATH") %>%
  file.path(
    ., "fintech.trading.competition", "vignettes", "articles",
    fsep = "\\"
  ) %>%
  list.files(
    path       = .,
    pattern    = "^DU([0-9]*)\\.Rmd$"
  ) %>%
  gsub(".Rmd$", "", .) %>%
  setdiff(keepers) %>%
  purrr::walk(
    function(to_delete){
      paste0(to_delete, ".Rmd") %>%
        file.path(
          Sys.getenv("APP_BASE_PATH"),
          "fintech.trading.competition",
          "vignettes",
          "articles",
          .,
          fsep = "\\"
        ) %>%
        file.remove()
    }
  )
