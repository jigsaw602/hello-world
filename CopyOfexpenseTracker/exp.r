transactions_dt = data.table(readxl::read_xlsx("Transactions.xlsx"))
setnames(transactions_dt, "Luxury/Essential", "Luxurious")
transactions_dt = setorder(transactions_dt, "MonthYear")
monthwise_report = transactions_dt[,.(Amt = sum(Amount)), by = c("MonthYear", "Type","Account")]

timeline = function(grouping_variable, bin_list, plot_type){
  #grouping variable could be Account, Luxurious, Category
  reqd_dt = transactions_dt[Luxurious != "NA" & Type == "Debit"][,.(Expense = sum(Amount)), by = c("MonthYear",grouping_variable )]
  reqd_dt = reqd_dt[eval(parse(text = grouping_variable)) %in% bin_list]
  #setnames(reqd_dt, "parse", grouping_variable)
  dt_list = list()
  for(i in unique(reqd_dt[[grouping_variable]])){
    temp = reqd_dt[eval(parse(text = grouping_variable)) == i][,!c(grouping_variable), with = FALSE]
    setnames(temp, "Expense",i)
    dt_list[[i]] = temp
  }
  final_dt = Reduce(function(x,y)merge(x,y,all=TRUE), dt_list)
  #reqd_dt$Date = as.Date(paste0(reqd_dt$MonthYear,"01"), format = "%Y%m%d")
  plot = highchart() %>% hc_chart(type = plot_type) %>%
    hc_xAxis(categories = final_dt$MonthYear)
  for( i in unique(reqd_dt[[grouping_variable]])){
    plot = plot %>% hc_add_series(data = final_dt[[i]], name = i)
  }
  plot = plot %>% hc_tooltip(shared = TRUE, split=FALSE) %>% hc_title(text = grouping_variable)
  return(list(plot = plot, data = final_dt))
}

saving_report = function(account){
  reqd_dt = monthwise_report[Account == account]
  dt_list = list()
  for(i in c("Debit", "Credit")){
    temp  = reqd_dt[Type == i][, !c("Type"), with=FALSE]
    setnames(temp, "Amt", i)
    dt_list[[i]] = temp
  }
  final_dt = Reduce(function(x,y)merge(x,y,all=TRUE), dt_list)
  final_dt$Savings = final_dt$Credit - final_dt$Debit
  plot = highchart() %>% hc_chart(type = "column") %>%
    hc_xAxis(categories = final_dt$MonthYear)
  for( i in c("Credit","Debit","Savings")){
    plot = plot %>% hc_add_series(data = final_dt[[i]], name = i)
  }
  plot = plot %>% hc_tooltip(shared = TRUE, split=FALSE) %>% hc_title(text = account)
  return(list(plot = plot, data = final_dt))
}

#Transactions function
#Will take input as month and then an input depending upon the metric, thereafter month of interest

transaction_list = function(metric, bin_list, months){
  return(transactions_dt[eval(parse(text = metric)) %in% bin_list & MonthYear %in% months])
}

#monthwise pie chart for the selected metric

month_pie = function(month, metric){
  reqd_dt = transactions_dt[Category != "NA" & MonthYear == month & Type == "Debit"][,.(Expense = sum(Amount)), by = c(metric)]
  p = highchart() %>% hc_chart(type = "pie") 
  if(metric == "Category"){
    p = p %>% hc_add_series(reqd_dt,"pie",hcaes(x = Category, y = Expense))
  }
  else if(metric == "Luxurious"){
    p = p %>% hc_add_series(reqd_dt,"pie",hcaes(x = Luxurious, y = Expense))
  }
  else{
    p = p %>% hc_add_series(reqd_dt,"pie",hcaes(x = Account, y = Expense))
  }
    
    
  return(list(
    plot = p,
    data = reqd_dt
  ))
  
    
  
}

summary_total = function(month){
  income_dt = transactions_dt[ Description == "Salary"][,.(MonthYear,Amount)]
  setnames(income_dt,"Amount","Salary")
  
  expense_by_account = transactions_dt[grepl("Transfer", Description)== FALSE & Type == "Debit" & Category != "NA"][,.(Expense = sum(Amount)), by = c("Account")]
  expense_by_monthyear = transactions_dt[grepl("Transfer", Description)== FALSE & Type == "Debit" & Category != "NA"][,.(Expense = sum(Amount)), by = c("MonthYear")]
  income_expense_dt = merge(income_dt, expense_by_monthyear, all = TRUE)
  income_expense_dt$Savings = income_expense_dt$Salary - income_expense_dt$Expense
}
