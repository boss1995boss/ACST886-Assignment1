#Assignment1-Q2
#b
npv_scott = function(n){
  interest_rate = 0.035
  borrow_amount = 600000
  offset_account = 100000*3.5/100
  P = 32622.8
  cashflow = rep(-P-offset_account, n)
  cashflow[n] = cashflow[n] - 100000
  discount_cashflow = c(borrow_amount, cashflow/(1+interest_rate)^(1:n))
  npv_scott = sum(discount_cashflow)
}
#Find the years need to repay the load under the large repayments.
N = uniroot(npv_scott, c(10,30))
round(N[[1]],0)

##This show that the time need to repay the loan is between 22 and 23 years
npv_scott(22)*npv_scott(23)<0

## 22 < N < 23 means that it needs 22 regular payment and one smaller payment at the end
##Find the book value of the loan after 22 instalments
instalment = 36122.8;borrow_amount = 600000; i = 3.5/100
Book_value = borrow_amount*(1+i)^22 - sum(rep(instalment, 22)*(1+i)^(21:0))
Book_value
Final_payment = Book_value * (1+i)
Final_payment - 100000 -3500
