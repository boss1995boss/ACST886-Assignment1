#Assignment1-Q1

npv_bill = function(P){
  interest_rate = 0.035
  borrow_amount = 600000
  cashflow = rep(-P, 30)
  discount_cashflow = c(borrow_amount, cashflow/(1+interest_rate)^(1:30))
  npv_bill = sum(discount_cashflow)
}
P = uniroot(npv_bill, c(30000,40000))
payment = P[[1]]
round(payment,2)
