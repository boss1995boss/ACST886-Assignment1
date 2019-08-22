#Assignment1-Q3
#a
i  = 3.5/100
cashflow = rep(1000, 30)
pv = sum(cashflow/(1+i)^(1:30))
pv

#c
npv_irr = function(i){
  borrow_amount = 600000
  P = payment-1000
  cashflow = rep(-P, 30)
  discount_cashflow = c(borrow_amount, cashflow/(1+i)^(1:30))
  npv_irr = sum(discount_cashflow)
}
IRR = uniroot(npv_irr, c(0.03, 0.035))
i = IRR[[1]]
i

#d
npv_bill2 = function(P){
  interest_rate = i
  borrow_amount = 600000
  cashflow = rep(-P, 30)
  discount_cashflow = c(borrow_amount, cashflow/(1+interest_rate)^(1:30))
  npv_bill2 = sum(discount_cashflow)
}
P = uniroot(npv_bill2, c(25000,35000))
P[[1]]
