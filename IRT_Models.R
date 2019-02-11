
# ------------------------------------------------------------------------------------------
# adopted from 
# https://rstudio-pubs-static.s3.amazonaws.com/357155_6674780326ef4afba5f009d17a85d4ae.html
# prepared by: Nahil A. Sobh on 2/11/19
# ------------------------------------------------------------------------------------------

library("mirt")

# **************************************************************************
               # =====================================
               # -              D A T A              -
               # =====================================
# **************************************************************************
#-----------------------------------------------------------------------------
# Synthetic Data to be replaced with  DATA relevantg to your application
# Here we generated 2 items (i.e. questions) with 100 users.
# Each question has 5 categories
#----------------------------------------------------------------------------
set.seed(3)

number_of_categories = 5
number_of_users      = 100
number_of_items      = 2
generated_data=replicate( number_of_items, rbinom(number_of_users, number_of_categories, prob=0.7) )

colnames(generated_data)=c('q1','q2')
head(generated_data)

# **************************************************************************
               # =====================================
               # -        I R T   M O D E L S        -
               # =====================================
# **************************************************************************
# -------------------------------------
# 1. Partial Credit Model             -
#    Specifies that each item has its own rating scale structure. 
#    It derives from multiple-choice tests where responses that are incorrect, 
#    but indicate some knowledge, are given partial credit towards a correct response. 
#    The amount of partial correctness varies across items.
# -------------------------------------
results.pcm  <- mirt( generated_data, 1, itemtype='Rasch', verbose=FALSE, SE=TRUE )
coef.pcm     <- coef( results.pcm, IRTpars=TRUE, simplify=TRUE )
items.pcm    <- as.data.frame( coef.pcm$items )
print(items.pcm)

# -------------------------------------
# 2. Generalized Partial Credit Model -
# -------------------------------------
results.gpcm <- mirt( generated_data, 1, itemtype='gpcm', verbose=FALSE, SE=TRUE )
coef.gpcm    <- coef( results.gpcm, IRTpars=TRUE, simplify=TRUE )
items.gpcm   <- as.data.frame( coef.gpcm$items )
print(items.gpcm)

# -------------------------------------
# 3. Rating Scale Model               -  
#    Specifies that a set of items share the same rating scale structure. 
#    It originates in attitude surveys where the respondent is presented 
#    the same response choices for several items.
# -------------------------------------
results.rsm  <- mirt( generated_data, 1, itemtype='rsm',  verbose=FALSE, SE=TRUE )
coef.rsm     <- coef( results.rsm, IRTpars=TRUE, simplify=TRUE )
items.rsm    <- as.data.frame( coef.rsm$items )
print(items.rsm)

# -------------------------------------
# 4. Graded Response Model            -
#    Handles ordered categories  A,B,C,D,E
# -------------------------------------
results.grm  <- mirt( generated_data, 1, itemtype='graded', verbose=FALSE, SE=TRUE )
coef.grm     <- coef( results.grm, IRTpars=TRUE, simplify=TRUE )
items.grm    <- as.data.frame( coef.grm$items )
print(items.grm)

# -------------------------------------
# 5. Nominal Model                    -
#    Handles Unordered categories 
# -------------------------------------
results.nominal <- mirt( generated_data, 1, itemtype='nominal', verbose=FALSE, SE=TRUE )
coef.nominal    <- coef( results.nominal, IRTpars=TRUE, simplify=TRUE )
items.nominal   <- as.data.frame( coef.nominal$items )
print(items.nominal)

# **************************************************************************
               # =====================================
               # -      V i s u a l i z a t i o n    -
               # =====================================
# **************************************************************************
# -------------------------------------
# 1. Graded Response Model            -
# -------------------------------------
# ---
plot( 
      results.grm, type='trace', which.items=c(1, 2), 
      main="Graded Response Model", par.settings=simpleTheme(lty=1:4,lwd=2),
      auto.key=list(points=FALSE,lines=TRUE, columns=4) 
    ) 

# ---
plot( 
      results.grm, type='infotrace', which.items=c(1, 2), 
      main="Graded Response Model", par.settings=simpleTheme(lwd=2)
     )

# ---
plot( results.grm, type='info', theta_lim=c(-4,4), lwd=2 )  

#---
plot( results.grm, type='SE' , theta_lim=c(-4,4), lwd=2 ) 


# -------------------------------------
# 1. Nominal Response Model          -
# -------------------------------------
# ---
plot( 
  results.nominal, type='trace', which.items=c(1, 2), 
  main="Nominal Response Model", par.settings=simpleTheme(lty=1:4,lwd=2),
  auto.key=list(points=FALSE,lines=TRUE, columns=4) 
) 

# ---
plot( 
  results.nominal, type='infotrace', which.items=c(1, 2), 
  main="Nominal Response Model", par.settings=simpleTheme(lwd=2)
)

# ---
plot( results.nominal, type='info', theta_lim=c(-4,4), lwd=2 )  

#---
plot( results.nominal, type='SE' , theta_lim=c(-4,4), lwd=2 ) 




