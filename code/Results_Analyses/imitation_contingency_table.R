contingency_table <- table(data_no_missing$q21_copy_you, data_no_missing$q34_copy_actions)
rownames(contingency_table) <- c("q21_copy_you=0","q21_copy_you=1")
colnames(contingency_table) <- c("q34_copy_action=0", "q34_copy_action=1")
print(contingency_table)
