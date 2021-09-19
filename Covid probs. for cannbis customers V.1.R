# PARAMETERS INTRODUCED:  pc(i), m, pc[i], pa[i] = m*pc(i), N, inf, p 
# Use Ps as the variable to control the range of the three prob, m, p, and inf.

Ps <- 30

#GOAL:  get an idea of how likely it is for the store to shut down due to covid infection resulting from uunmasked customers as opposed to masked customers.  Assume all employees are masked and all dealing with customers are behind shields.

#Let pc be the vector of customer contagion probabilities for N unmasked customers.
#Let pc[i] be the probability that a random unmasked cannabis customer i is covid contagious.
#Assume pc[i] and pc[j] are independent for all i != j.    ! is negation.

#Let 0 ≤ m ≤ 1 be the mask effect such that the actual contagion probability for masked person i is reduced to m*pc[i] = pa[i].

# SIMPLIFYING ASSUMPTION 1.  ASSUME pc[i] = p for all i.  If customer is wearing a mask, pa[i] = mp
# SIMPLIFYING ASSUMPTION 2.  ASSUME A CUSTOMER WORKS WITH ONLY ONE EMPLOYEE.  IGNORES SECURITY CONTACTS BEFORE ENTRANCE TO STORE.
# SA 3.  ALL EMPLOYEES ARE VACCINATED.
# SA 4.  ASSUME PROBABILITY OF EMPLOYEE BEING INFECTED GIVEN A CUSTOMER IS CONTAGIOUS IS inf 
# SA 5.  Assume no customer wears a mask unless required to.  Introducing a fraction of customers who do wear a mask even though not required is equivalent to changing the value of m.

# QUESTION 1:  WHAT CIRCUMSTANCES REQUIRE STORE SHUTDOWN:  EMPLOYEE SYMPTOMS?  EMPLOYEE POSITIVE TEST?  HOW MANY EMPLOYEES?
#.QUESTION 2:  WHAT CIRCUMSTANCES REQUIRE EMPLOYEE QUARANTINE BUT STORE REMAINS OPEN?
 

# For any single customer i wearing a mask, the probability that an employee will be infected is Pr(customer is contagious)*Pr(employee is infected goven customer is contagious) = mp*inf.  A perfect mask would have m = 0, a useless mask would have m = 1.

# For any single customer i not wearing a mask, the probability that an employee will be infected is Pr(customer is contagious)*Pr(employee is infected goven customer is contagious) = p*inf

# For N masked customers the probability of at least one infected employee = one minus the probability of no infections.
# The probability of no infections, assuming independence, is (1 - mp*inf)^N	
#  Therefore the probability of at least one infections is Pm <- 1 - (1 - mp*inf)^N	  	
# For N unmasked customers the probability of at least one infected employee = one minus the probability of no infections.
# The probability of no infections, assuming independence, is (1 - p*inf)^N										
# Therefore the probability of at least one infections is  Pnm <-  1 - (1 - p*inf)^N 	  

# The relative likelihood of at least one infection resulting from no mask compared to at least one infection resulting from masking of customers is Pnm/Pm.

# EXAMPLE

p <- .05  # CONTAGION PROB.  Probability that customer is contagious.
m <- .10  # MASK EFFECT. The fraction by which the contagious effect of the customer is reduced.
N <- 20  # NUMBER OF CUSTOMERS
inf <- .02 # PROB. OF MASKED EMPLOYEES GETTING INFECTED BY CONTAGIOUS CUSTOMER.  A VULNERABILITY MEASURE THAT DOES NOT DEPEND ON WHETHER OR NOT THE CUSTOMER IS MASKED.

p*inf # The probability a given employee is infected by an unmasked customer
p*m*inf   # The probability a given employee is infected by a masked customer
(1 - m*p*inf)  # The probability that an employee is not infected by a masked customer
(1 - p*inf)  # The probability that an employee is not infected by an unmasked customer
(1 - m*p*inf)^N  # # The probability that an employee is not infected by any of N masked customers.
(1 - p*inf)^N  # # The probability that an employee is not infected by any of N unmasked customers. 
Pm <- 1 - (1 - m*p*inf)^N  #  The probability that at least one of the masked customers infects one of the employees
Pnm <-  1 - (1 - p*inf)^N  #  The probability that at least one of the unmasked customers infects one of the employees

Pm <- 1 - (1 - m*p*inf)^N 
Pnm <-  1 - (1 - p*inf)^N 
Pnm/Pm

print(paste("It is ", round(Pnm/Pm, digit = 2), " times as likely that at least one employeed will get infected by ", N, " unmasked customers as by ", N,  " masked customers.") )
Pnm
Pm
#*****************************************************************************
Relative <- function(p, m, N, inf){(1 - (1 - p*inf)^N)/(1 - (1 - m*p*inf)^N)}

Relative2 <- function(p, m, N, inf){(p*inf)/(m*p*inf)}



Reln <-  rep(0, 5000)
Rp <-Rm <- Rinf <- Relp <- rep(0, Ps)

for(N in c(1:5000)){Reln[N] <- Relative(.05, m, N, inf)}
N <- 500
for (inf in c(1:Ps)){Rinf[inf] <- Relative(.05, .10, N, inf/100)}
for (RM in c(1:Ps)){Rm[RM] <- Relative(.05, RM/100, 500, .02)}
for (RP in c(1:Ps)){Rp[RP] <- Relative(.05, RP/100, 500, .02)}
# op <- par(mfrow = c(2,2))
plot(c(1:5000), Reln, xlab = "N = Number of Customers", ylab = "Contagion Ratio, No mask to Mask", main = "Defaults = (p, m, N, inf) = (.05, .10, 500, .02)")  # As a fcn of N.  For very large N the contagion ratio asymptotes at 1.00.

plot(rev(c(1:Ps)/100), Rinf, xlab = "p = Infection Probability", ylab = "Contagion Ratio, No mask to Mask", main = "Defaults = (p, m, N, inf) = (.05, .10, 500, .02)")  # As a fcn of inf

plot(rev(c(1:Ps)/100), Rm, xlab = "m = Mask Effectiveness", ylab = "Contagion Ratio, No mask to Mask", main = "Defaults = (p, m, N, inf) = (.05, .10, 500, .02)")  # As a fcn of m


plot(rev(c(1:Ps)/100), Rp, xlab = "p = Contagion Probability", ylab = "Contagion Ratio, No mask to Mask", main = "Defaults = (p, m, N, inf) = (.05, .10, 500, .02)")  # As a fcn of p


# Default values
p <- .05  # CONTAGION PROB.
m <- .10  # MASK EFFECT
N <- 100  # NUMBER OF CUSTOMERS
inf <- .02 # PROB. OF MASKED EMPLOYEES GETTING INFECTED BY INFECTED CUSTOMER

# Sequence values
RN <- seq(1, 501, 20)
Rinf <- seq(.01, 1, 0.01) 
Rm  <- seq(.01, 1, 0.01)
Rp <- seq(.01, 1, 0.01) 



# Relative risk for mask effectiveness and infection rate
f     <- function(Rinf, Rm) Relative(p, Rm, N, Rinf)
z     <- outer(Rinf, Rm, f)
persp(Rinf, Rm, z, theta = -30, phi = 25, shade = 0.05, col = "white", expand = 0.5, r = 2, ltheta = 25, ticktype = "detailed")


# Relative risk for mask effectiveness and contagion probability
f     <- function(Rinf, Rp) Relative(Rp, m, N, Rinf)
z     <- outer(Rinf, Rp, f)
persp(Rinf, Rp, z, theta = -30, phi = 25, shade = 0.05, col = "white", expand = 0.5, r = 2, ltheta = 25, ticktype = "detailed")


# Relative risk for mask effectiveness and infection rate
f     <- function(Rinf, Rm) Relative(p, Rm, N, Rinf)
z     <- outer(Rinf, Rm, f)
persp(Rinf, Rm, z, theta = -30, phi = 25, shade = 0.05, col = "white", expand = 0.5, r = 2, ltheta = 25, ticktype = "detailed")


# Relative risk for mask effectiveness and contagion probability
f     <- function(Rinf, Rp) Relative(Rp, m, N, Rinf)
z     <- outer(Rinf, Rp, f)
persp(Rinf, Rp, z, theta = -30, phi = 25, shade = 0.05, col = "white", expand = 0.5, r = 2, ltheta = 25, ticktype = "detailed")


# Relative risk for mask effectiveness and infection rate
f     <- function(Rinf, Rm) Relative(p, Rm, N, Rinf)
z     <- outer(Rinf, Rm, f)
persp(Rinf, Rm, z, theta = -30, phi = 25, shade = 0.05, col = "white", expand = 0.5, r = 2, ltheta = 25, ticktype = "detailed")


# Relative risk for mask effectiveness and contagion probability
f     <- function(Rinf, Rp) Relative(Rp, m, N, Rinf)
z     <- outer(Rinf, Rp, f)
persp(Rinf, Rp, z, theta = -30, phi = 25, shade = 0.05, col = "white", expand = 0.5, r = 2, ltheta = 25, ticktype = "detailed") 




