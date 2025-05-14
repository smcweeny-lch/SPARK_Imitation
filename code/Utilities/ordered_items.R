votes <- readxl::read_xlsx("./construct_voting/Item_Voting_Ericka_12_29_24.xlsx")
ordered <- !votes$from %in% c("rbs-r", "dcdq")
ordered_names <- votes$name[ordered == T]
rm(votes)
