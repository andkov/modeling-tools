

sample_of_interest1 <-  4187213
# ---- make-treatment-functions ------------------------------------------------------------
# adds a binary variable 'tx', encoding the presence or absence of the intervention
make_treatment_binary <- function(
    d
    ,tx_var     # one of the intervention variable used to create a binary treatment variable
){
  # d <- ds1
  d1 <- 
    d %>% 
    mutate(
      !!rlang::sym(tx_var) :=
        ( !!rlang::sym(tx_var) %>% as.character() %>% as.integer() ) > 0L
    )
}

# replaces the target variable with a binary variable `tx`
create_treatment_binary <- function(
    d
    ,tx_var     # one of the intervention variable used to create a binary treatment variable
){
  d1 <- 
    d %>% 
    mutate(
      tx = (!!rlang::sym(tx_var) %>% as.character() %>% as.integer()) > 0L
    )  %>% 
    select(-!!rlang::sym(tx_var)) 
}
# How to use:
# ds5 %>%
#   filter(person_oid %in% sample_of_interest1) %>%
#   create_treatement_binary("career_planning") %>%
#   select(-person_oid) %>%
#   glimpse()



