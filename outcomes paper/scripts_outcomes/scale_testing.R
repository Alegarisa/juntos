
### Libraries
library(rio)
library(here)
library(tidyverse)
library(haven)
library(janitor)
library(knitr)
library(surveytoolbox)
library(sjPlot)
library(kableExtra)
library(psych)
library(sjmisc)
library(MVN)

### The dataset ###
parent_w1 <- read_sav(here("outcomes paper/data_outcomes", "primary_parent_w1.sav"))

################################################
# From Flora, LaBrish, 
################################################
# Check collineraity: "refers to perfect or near-perfect linear relationships among observed variables. With multiple regression, the focus is on collinearity among explanatory variables, but with factor analysis, the concern is collinearity among dependent variables,that is, the set of variables being factor analyzed. When collinear variables are included, the product-moment correlation matrix R will be singular, or non-positive definite.
# Collinearity in factor analysis is relatively simple to diagnose: if any eigen values of a product-moment R equal zero or are negative,then R is non-positive definite and collinearity is present(and software will likely produce a warning message).

# Heywood cases: "Improper solutions (e.g.,a model solution with at least one negative estimated residual variance term, or“Heywoodcase”) are also likely to occur in the presence of one or more unusual cases (Bollen, 1987), which can lead to a researcher unwittingly revising a model or removing an observed variable from the analysis."

# # #
# multicollenearity will produce 1 of 2 things:
# - aberrant factorial load 
# - negative variance (more for CFA) - non-positive definite matrix. 
# 1. Check for collinearity between items included on a EFA model using the correlation plot
################################################
# Steps
# 1. matrix polychoric
# 2. kmo
# 3. parallel analysis (with no warning messages)
# if it shows more factors than sensible: look for communality above .4 (h2); factor loading above 0.4; 

#######################################################################################
############################# Positive relationship ###################################
#######################################################################################

### The items ###
# 116.	Cuando mi joven me pide hablar o cuando necesita hablar conmigo, escucho atentamente.
# 117.	Sé escuchar atentamente, aun cuando no esté de acuerdo con lo que dice la otra persona.
# 118.	Con regularidad, mi joven y yo hacemos cosas juntos que ambos disfrutamos
# 119.	Mi joven y yo tenemos una relación cercana
# 120.	Hago y digo cosas que le muestran a mi joven que a él/ella me importa y que yo la/lo amo (por ejemplo, ...)
# 121.	Como madre/padre, es mi trabajo reconocer y apoyar las fortalezas de mi joven

# The scale to factor analyze
posi_rel <- parent_w1 %>%
  select(q116, q117, q118, q119, q120, q121)

# Poly corr matrix
poly <- polychoric(posi_rel)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(posi_rel) # 0.88 ok 

### EFA analyses: using oblimin: it's best to assume that factors are correlated.  
fa.parallel(poly$rho, n.obs=95, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  1
# shows 4 warning messages: "The estimated weights for the factor scores are probably incorrect." and "An ultra-Heywood case was detected" 

fa.parallel(poly$rho, n.obs=95, fm="pa", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  1
# shows 7 warning messages: "The estimated weights for the factor scores are probably incorrect." and "An ultra-Heywood case was detected"

# EFA using ULS 
factor_test_uls <- fa(posi_rel, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# Model above throws warning message: "The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method."

fa.diagram(factor_test_uls) # 119 and 120 have really high factorial loads 

# EFA using PA
factor_test_pa <- fa(posi_rel, n.obs = 95, rotate = "oblimin", fm = "pa", cor = "poly", nfactors = 1) 
# Model above throws warning message: "The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method."

fa.diagram(factor_test_pa) # 119 and 120 have really high factorial loads 

#### Conclusion here: estimation method (uls or pa) shows the same factor loadings. Item 120 has a very high factor loading (0.99) and by checking the poly cor matrix, item 120 shows that it has very high correlations with all items

# Removing item 120

# The scale to factor analyze
posi_rel_2 <- parent_w1 %>%
  select(q116, q117, q118, q119, q121)

# Poly corr matrix
poly <- polychoric(posi_rel_2)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # The correlations are not as elevated

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(posi_rel_2) # 0.85

# parallel test with ULS
fa.parallel(poly$rho, n.obs=95, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  1, BUT it throws warning messages: "Try a different factor score estimation method" and "An ultra-Heywood case was detected" ------- BTW: the number of warning messages change everytime i run the code. 

# parallel test with PA
fa.parallel(poly$rho, n.obs=95, fm="pa", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE) # Parallel analysis suggests that the number of factors =  1, BUT it throws warning messages: "Try a different factor score estimation method" and "An ultra-Heywood case was detected" ------- BTW: the number of warning messages change everytime i run the code. 

# EFA using ULS 
factor_test_uls <- fa(posi_rel_2, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# no warning message :)

fa.diagram(factor_test_uls) # item 119 has a factor loading of 1

    ULS1   h2     u2 com
q116 0.92 0.84 0.1606   1
q117 0.84 0.71 0.2907   1
q118 0.83 0.69 0.3123   1
q119 1.00 0.99 0.0093   1 ##
q121 0.81 0.66 0.3402   1

# EFA using pa 
# factor_test_pa <- fa(posi_rel_2, n.obs = 95, rotate = "oblimin", fm = "pa", cor = "poly", nfactors = 1) # no warning message :)
# 
# fa.diagram(factor_test_pa) # item 119 has a factor loading of 1

# alpha
alpha(posi_rel_2) # 0.89

# trying without 119 (check for SS loadings and Proportion Var (larger # better); and TLI larger, better)

# The scale to factor analyze
posi_rel_3 <- parent_w1 %>%
  select(q116, q117, q118, q121)

# Poly corr matrix
poly <- polychoric(posi_rel_3)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) 

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(posi_rel_3) # 0.81

# parallel test with ULS
fa.parallel(poly$rho, n.obs=95, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  2, and STILL throws warning messages: "Try a different factor score estimation method" and "An ultra-Heywood case was detected" ------- BTW: the number of warning messages change everytime i run the code. 

# parallel test with PA
fa.parallel(poly$rho, n.obs=95, fm="pa", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE) 
# Parallel analysis suggests that the number of factors =  2, and STILL it throws warning messages: "Try a different factor score estimation method" and "An ultra-Heywood case was detected" ------- BTW: the number of warning messages change everytime i run the code. 
# ######################## IF i USE posi_rel_3, n.obs=NULL, I GET JUST 1 FACTOR #### I don't understand this shit. 

# EFA using ULS 
factor_test_uls <- fa(posi_rel_3, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) # no warning message :)

fa.diagram(factor_test_uls)

# EFA using pa 
factor_test_pa <- fa(posi_rel_3, n.obs = 95, rotate = "oblimin", fm = "pa", cor = "poly", nfactors = 1) # no warning message :)

fa.diagram(factor_test_pa)

      ULS1   h2   u2 com
q116 0.93 0.87 0.13   1
q117 0.84 0.70 0.30   1
q118 0.82 0.67 0.33   1
q121 0.81 0.66 0.34   1

# Conclusion: I have to ignore the parallel analysis suggestion of 2 factors, because there is just 4 items. 
# 
# SOOOO... i AM GOING TO KEEP ITEM 119 BECAUSE MAKES THE SOLUTION MORE INESTABLE.
# IDEAL QUE NO HAYA WARNINGS EN PARALLEL, PERO SI SALEN, NI MODO. DONDE SI NO SE PUEDEN TENER WARNINGS ES EN EL fa()

# Final items:
# 116.	Cuando mi joven me pide hablar o cuando necesita hablar conmigo, escucho atentamente.
# 117.	Sé escuchar atentamente, aun cuando no esté de acuerdo con lo que dice la otra persona.
# 118.	Con regularidad, mi joven y yo hacemos cosas juntos que ambos disfrutamos
# 119.	removed
# 120.	removed
# 121.	Como madre/padre, es mi trabajo reconocer y apoyar las fortalezas de mi joven

# alpha
alpha(posi_rel_3) # 0.84

# Omega 
omega(posi_rel_3, nfactors = 1, n.obs = 95, flip = T, plot = T) # likes more than 1 factor, but still good to know Omega Total 0.84 (same as alpha)

# new thoughts on 08/14/23: I am not sure if I want to leave out item 119 (that says Mi joven y yo tenemos una relación cercana) 
# 5 items alpha = 0.89 
# 4 items alpha = 0.84 

# More than an alpha aspect is the content of the item. I think I will create 2 variables and see how they behave in multiple regression. 
############ CFA ##############

# el siguiente paso seria usar lavaan, especificar el modelo 
rp <- ("rp=~ q116 + q117 + q118 + q119 + q121") # aqui necesito un long format (116 seria variable y row seria tiempo 1, 2 y 3)


# [TBD]

#######################################################################################
############################# positive involvement ####################################
#######################################################################################

# # this scale is dichotomous
# 
# ### The items ###
# # 1.	Trabajamos en un pasatiempo o artesanía.
# # 2.	Participamos en una actividad al aire libre.
# # 3.	Leímos o hablamos acerca de un libro o historia.
# # 4.	Fuimos a un evento de entretenimiento.
# # 5.	Participamos en otras actividades (Fuimos al parque, nadamos, excursión a pie, etc.). 
# # 6.	Horneamos o cocinamos una comida.
# # 7.	Hicimos ejercicio o jugamos un juego al aire libre (baloncesto o béisbol, etc.)
# # 8.	Trabajamos alrededor de la casa o patio.
# # 9.	Fuimos a la Iglesia, sinagoga, u otro servicio religioso.
# 
# # leaving 8 and 9 out because seem out of concept #kmo with was 0.6
# 
# # The scale to factor analyze
# posi_inv <- parent_w1 %>%
#   select(q1, q2, q3, q4, q5, q6, q7)
# 
# ## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
# KMO(posi_inv) # 0.56 (ver 1 = pretty poor)
# 
# # Poly corr matrix
# poly <- tetrachoric(posi_inv) # because binary variables
# rho <- poly$rho
# cor.plot(rho, numbers = T, upper = F, main = "Tetrachoric", show.legend = F) # very low correlations
# 
# 
# fa.parallel(rho, n.obs=95, fm="uls", fa="both", main="Parallel Analysis Scree Plots", cor="tet", use="pairwise", plot=TRUE) # says 2 factors, but kmo is not good enough; 
# 
# # EFA using ULS 
# factor_test_uls <- fa(rho, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "tet", nfactors = 1) 
# 
# # ULS1    h2   u2 com
# q1 0.37 0.134 0.87   1
# q2 0.63 0.403 0.60   1
# q3 0.16 0.027 0.97   1
# q4 0.19 0.038 0.96   1
# q5 0.70 0.490 0.51   1
# q6 0.25 0.061 0.94   1
# q7 0.79 0.621 0.38   1
# 
# posi_inv <- parent_w1 %>%
#   select(q1, q2, q5, q7)
# 
# ## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
# KMO(posi_inv) # 0.64 better
# 
# # EFA using ULS 
# factor_test_uls <- fa(posi_inv, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "tet", nfactors = 1)
# # no warnings
# 
# #  ULS1    h2   u2 com
# q1 0.31 0.095 0.90   1
# q2 0.62 0.382 0.62   1
# q5 0.71 0.507 0.49   1
# q7 0.83 0.694 0.31   1
# 
# # Omega 
# omega(posi_inv, nfactors = 1, n.obs = 95, flip = T, plot = T) # 0.57
# 
# # Conclusion: evidence of construct validity, but it has poor internal consistency (relation between items is not very high).  
# 
# # make a CFA using wave 2 and 3, to check time invariance. [TBD]


#######################################################################################
############################# Monitoring ##############################################
#######################################################################################

### The items ###
# 137.	A menudo hablo con mi joven acerca de sus planes para el día siguiente
# 138.	Hablo con mi joven en muchas ocasiones acerca de lo que él/ella aprendió en la escuela
# 139.	A menudo hablo con mi joven acerca de sus amigos. 
# 140.	Conozco muy bien a los amigos de mi joven. 
# 141.	Los amigos de mi joven tienen una buena influencia en su vida. 
# 142.	Los amigos de mi joven se apoyan positivamente entre sí.
# 143.	Por lo general yo sé con quién está mi joven
# 144.	Sé lo que hace mi joven y dónde va cuando no está en casa

# The scale to factor analyze
monit <- parent_w1 %>%
  select(q137, q138, q139, q140, q141, q142, q143, q144)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(monit) # 0.78

# Poly corr matrix
poly <- polychoric(monit)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # looks good, items 137 and 144 have lower corrs. 

# parallel test, anyways
fa.parallel(monit, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  3, warnings about ultra-heywood case

# EFA using ULS 
factor_test_uls <- fa(monit, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# No warnings
# all factor loadings above 0.4

    ULS1   h2   u2 com
q137 0.49 0.24 0.76   1
q138 0.78 0.60 0.40   1
q139 0.67 0.45 0.55   1
q140 0.63 0.40 0.60   1
q141 0.59 0.35 0.65   1
q142 0.80 0.64 0.36   1
q143 0.78 0.60 0.40   1
q144 0.45 0.20 0.80   1

# EFA using ULS - More than 1 factors 
# factor_test_uls_2 <- fa(monit, n.obs = 95, rotate = "promax", fm = "pa", cor = "cor", nfactors = 2) # just for fun, to compare SPSS output and R output. 
# # Note 07.02.24: I noticed discrepancies in SPSS and R output. I figured out that discrepancies are resolved when SPSS is put to exclude cases pairwise. 
# I believe that is what R uses too. Discrepancies persist if I use uls, oblimin, with cor, which I thought were equivalent in SPSS

# EFA using ULS - More than 1 factors 
factor_test_uls_2 <- fa(monit, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2)
# No warnings

# factor 1
#     ULS1  ULS2   h2   u2 com
q140  0.75 -0.08 0.49 0.51 1.0
q141  0.71 -0.07 0.44 0.56 1.0
q142  0.79  0.08 0.71 0.29 1.0
q143  0.69  0.14 0.63 0.37 1.1

140.	Conozco muy bien a los amigos de mi joven.
141.	Los amigos de mi joven tienen una buena influencia en su vida.
142.	Los amigos de mi joven se apoyan positivamente entre sí.
143.	Por lo general yo sé con quién está mi joven

# factor 2
q138  0.13  0.76 0.73 0.27 1.1
q139  0.04  0.74 0.58 0.42 1.0
q144 -0.15  0.69 0.36 0.64 1.1

138.	Hablo con mi joven en muchas ocasiones acerca de lo que él / ella aprendió en la escuela
139.	A menudo hablo con mi joven acerca de sus amigos.
144.	Sé lo que hace mi joven y dónde va cuando no está en casa

# loads into both
q137  0.24  0.29 0.23 0.77 1.9

monit_2 <- parent_w1 %>%
  select(q138, q139, q140, q141, q142, q143, q144)

# EFA using ULS - 2 factors without item 137
factor_test_uls_2 <- fa(monit_2, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2)

      ULS1  ULS2   h2   u2 com

# Factor #1 : monitoring peers
q140  0.72 -0.08 0.46 0.54 1.0
q141  0.72 -0.06 0.47 0.53 1.0
q142  0.80  0.05 0.70 0.30 1.0
q143  0.69  0.17 0.65 0.35 1.1

# Factor #2 : checking in (?)
q138  0.23  0.64 0.65 0.35 1.2
q139  0.09  0.70 0.58 0.42 1.0
q144 -0.15  0.74 0.43 0.57 1.1

# Omega
omega(monit_2, nfactors = 2, n.obs = 95, flip = T, plot = T) # 0.78

monit_peers <- parent_w1 %>%
  select(q140, q141, q142, q143)
# alpha
alpha(monit_peers) # 0.76

# checkin <- parent_w1 %>%
#   select(q138, q139, q144)
# # alpha
# alpha(checkin) # 0.59 # too low. Keeping just monitoring peers. 

#######################################################################################
############################# Homework Involvement ####################################
#######################################################################################

### The items ###
# 82	Mi joven ha tenido la plena responsabilidad de completar su tarea; no me involucro.
# 83	Dejé de intentar (de ayudarlo con su tarea).
# 84	Me aseguré de que tenga un cierto tiempo para hacer la tarea
# 85	Me aseguré de que él / ella tenga un cierto lugar para hacer la tarea
# 86	Me senté con mi joven mientras él / ella hacía su tarea
# 87	Me hice disponible para mi joven cuando tenía preguntas sobre su tarea
# 88	Ayudé a mi joven cuando estaba atascado/a en una tarea
# 89	Ayudé a mi joven a prepararse para los exámenes haciéndole preguntas sobre el tema, etc.
# 90	Le di a mi joven incentivos como privilegios especiales, regalos especiales, etc.
# 91	Utilicé consecuencias, castigos, acciones disciplinarias
# 92	Le recordé a mi joven que hiciera su tarea
# 93	Comprobé con mi joven para asegurarme de que él / ella hiciera su tarea
# 94	Comprobé y corregí la tarea de mi hija/o 
# 95	Comprobé con la maestra/o para asegurarme que mi joven terminó su tarea 
# 96	Le ayudé a mi joven acceder a otros recursos (biblioteca, materiales de arte, laboratorio de computación, etc.)
# 97	Le ayudé a mi joven practicar habilidades (lectura, revisión, problemas de práctica, etc.)
# 98	Limité el ruido y/o las distracciones mientras hacia su tarea

# The scale to factor analyze
hw_inv <- parent_w1 %>%
  select(q84, q85, q86, q87, q88, q89, q90, q91, q92, q93, q94, q95, q96, q97, q98) # removed 82 and 83; almost all negative correlations, which make sense. 

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(hw_inv) # 0.71 --> ok

# Poly corr matrix
poly <- polychoric(hw_inv)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F)
# items 84, 89, 89 neg corr with other items

# 84	Me aseguré de que tenga un cierto tiempo para hacer la tarea
# 89	Ayudé a mi joven a prepararse para los exámenes haciéndole preguntas sobre el tema, etc.
# 89	Ayudé a mi joven a prepararse para los exámenes haciéndole preguntas sobre el tema, etc.

# scree plot
scree(hw_inv, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot shows 3 factor solution, but 1 eigenvalue would be less than 1.0

# EFA using ULS 1 factor
factor_test_uls <- fa(hw_inv, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 

# EFA using ULS 2 factors
factor_test_uls <- fa(hw_inv, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 

     ULS1  ULS2    h2   u2 com

# factor #1
q86  0.57  0.13 0.371 0.63 1.1
q87  0.74 -0.06 0.540 0.46 1.0
q88  0.84 -0.18 0.691 0.31 1.1
q89  0.64  0.18 0.482 0.52 1.1
q94  0.56  0.09 0.343 0.66 1.0
q96  0.51  0.09 0.282 0.72 1.1
q97  0.47  0.16 0.270 0.73 1.2

# factor #2
q84 -0.12  0.84 0.689 0.31 1.0
q85 -0.02  0.90 0.796 0.20 1.0
q93  0.19  0.70 0.566 0.43 1.1

#cross-loading
q90  0.38  0.28 0.262 0.74 1.8
q95  0.33  0.40 0.311 0.69 1.9
q98  0.30  0.36 0.258 0.74 1.9

# below .30
q91  0.16  0.23 0.091 0.91 1.8
q92  0.12  0.22 0.070 0.93 1.6


# Poly corr matrix
poly <- polychoric(hw_inv_1)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # looks better, but 84 and 85 show some neg corr with other items.

# new efa iteration without problematic items
hw_inv_1 <- parent_w1 %>%
  select(q84, q85, q86, q87, q88, q89, q93, q94, q96, q97)

# EFA using ULS 2 factors
factor_test_uls <- fa(hw_inv_1, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 

     ULS1  ULS2   h2   u2 com

q86  0.60  0.16 0.42 0.58 1.1
q87  0.73 -0.06 0.52 0.48 1.0
q88  0.88 -0.14 0.76 0.24 1.1
q89  0.66  0.20 0.51 0.49 1.2
q94  0.53  0.04 0.29 0.71 1.0
q96  0.50  0.08 0.26 0.74 1.1
q97  0.46  0.15 0.25 0.75 1.2

# 86	Me senté con mi joven mientras él / ella hacía su tarea
# 87	Me hice disponible para mi joven cuando tenía preguntas sobre su tarea
# 88	Ayudé a mi joven cuando estaba atascado/a en una tarea
# 89	Ayudé a mi joven a prepararse para los exámenes haciéndole preguntas sobre el tema, etc
# 94	Comprobé y corregí la tarea de mi hija/o 
# 96	Le ayudé a mi joven acceder a otros recursos (biblioteca, materiales de arte, laboratorio de computación, etc.)
# 97	Le ayudé a mi joven practicar habilidades (lectura, revisión, problemas de práctica, etc.)

# factor # 1: academic hw
hw_inv_fa1 <- parent_w1 %>%
  select(q86, q87, q88, q89, q94, q96, q97)
# alpha
alpha(hw_inv_fa1) # 0.78

q84 -0.08  0.95 0.89 0.11 1.0
q85  0.04  0.90 0.83 0.17 1.0
q93  0.22  0.59 0.43 0.57 1.3

# 84	Me aseguré de que tenga un cierto tiempo para hacer la tarea
# 85	Me aseguré de que él / ella tenga un cierto lugar para hacer la tarea
# 93	Comprobé con mi joven para asegurarme de que él / ella hiciera su tarea

# factor # 2: checkhwi
hw_inv_fa2 <- parent_w1 %>%
  select(q84, q85, q93)
# alpha
alpha(hw_inv_fa2) # 0.77

# Omega 
omega(hw_inv_1, nfactors = 2, n.obs = 95, flip = T, plot = T) # alpha = 0.76
# message: Three factors are required for identification -- general factor loadings set to be equal. 
# Proceed with caution. 
# Think about redoing the analysis with alternative values of the 'option' setting.

#######################################################################################
############################# Discipline & limit setting ##############################
#######################################################################################

### The items ###
# 128.	En casa, estamos de acuerdo con reglas claras sobre lo que mi joven puede y no puede hacer. 
# 129.	Mi joven sabe cómo voy a responder cuando hace algo malo cosas que no me gustan o lo que está en contra las reglas de la casa)
# 130.	Cada vez que mi joven hace algo mal, yo le respondo con una consecuencia específica (p. ej., una disciplina específica, quitándole privilegios, etc.) 
# 131.	Cuando mi joven hace algo mal, le grito o le insulto
# 132.	Puedo controlar mi enojo y mantenerme calmado/a cuando disciplino o discuto con mi joven cuando él / ella hace algo mal
# 133.	Cuando mi joven me desafía al no hacer lo que le pido, yo renuncio
# 134.	Cuando mi joven está aprendiendo un nuevo comportamiento (p. ej.: ser más responsable, estudioso/a u organizado/a), reconozco su progreso con, por ejemplo, un abrazo, una sonrisa o un pequeño regalo
# 135.	Cuando mi joven se enfrenta a un gran desafío o establece una meta, le ayudo a centrarse en los pequeños pasos para lograr esa meta.
# 136.	Cuando le doy una amenaza o advertencia a mi joven, frecuentemente no lo llevo a cabo

# The scale to factor analyze

discipline <- parent_w1 %>%
  select(q128, q129, q130, q131, q132, q133, q134, q135, q136) 

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(discipline) # 0.76

# Poly corr matrix
poly <- polychoric(discipline)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F)
# item 136 correlates neg and low except  131 and 133. These are ineffective limit setting practices

# 131.	Cuando mi joven hace algo mal, le grito o le insulto
# 133.	Cuando mi joven me desafía al no hacer lo que le pido, yo renuncio
# 136.	Cuando le doy una amenaza o advertencia a mi joven, frecuentemente no lo llevo a cabo

# scree plot
scree(discipline, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot shows 1 factor solution

# parallel test, anyways
fa.parallel(discipline, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  3, there are warnings

# EFA using ULS 1 factor
factor_test_uls <- fa(discipline, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# warnings
# items 131, 133, 136 load negatively and very low. 

# EFA using ULS 2 factors
factor_test_uls <- fa(discipline, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 
# warnings
# items 131 crossloads 

posi_discipline <- parent_w1 %>%
  select(q128, q129, q130, q132, q134, q135) 

# EFA using ULS 1 factor
factor_test_uls <- fa(posi_discipline, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
#warnings: Try a different factor score estimation method.

# EFA using pa 
factor_test_pa <- fa(posi_discipline, n.obs = 95, rotate = "oblimin", fm = "pa", cor = "poly", nfactors = 1)
#same warning

# removing 128 and 135 that have really high loadings
posi_discipline <- parent_w1 %>%
  select(q129, q130, q132, q134) 

# EFA using ULS 1 factor
factor_test_uls <- fa(posi_discipline, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
#warnings.

# removing 129 really high loading
posi_discipline <- parent_w1 %>%
  select(q130, q132, q134) 

# EFA using ULS 1 factor
factor_test_uls <- fa(posi_discipline, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# no warnings :) but items crossload - this is not a good scale. 

     ULS1   h2   u2 com
q130 0.34 0.12 0.88   1
q132 0.56 0.32 0.68   1
q134 0.66 0.43 0.57   1

# 130.	Cada vez que mi joven hace algo mal, yo le respondo con una consecuencia específica (p. ej., una disciplina específica, quitándole privilegios, etc.) 
# 132.	Puedo controlar mi enojo y mantenerme calmado/a cuando disciplino o discuto con mi joven cuando él / ella hace algo mal
# 134.	Cuando mi joven está aprendiendo un nuevo comportamiento (p. ej.: ser más responsable, estudioso/a u organizado/a), reconozco su progreso con, por ejemplo, un abrazo, una sonrisa o un pequeño regalo

# alpha
alpha(posi_discipline) # 0.47 - really poor

# Omega 
#makes no sense to run omega. 

# 131.	Cuando mi joven hace algo mal, le grito o le insulto
# 133.	Cuando mi joven me desafía al no hacer lo que le pido, yo renuncio
# 136.	Cuando le doy una amenaza o advertencia a mi joven, frecuentemente no lo llevo a cabo

neg_discipline <- parent_w1 %>%
  select(q131, q133, q136) 

# alpha
alpha(neg_discipline) # 0.49 - really poor

# 135.	Cuando mi joven se enfrenta a un gran desafío o establece una meta, le ayudo a centrarse en los pequeños pasos para lograr esa meta.
# 128.	En casa, estamos de acuerdo con reglas claras sobre lo que mi joven puede y no puede hacer. 
# 129.	Mi joven sabe cómo voy a responder cuando hace algo malo cosas que no me gustan o lo que está en contra las reglas de la casa)

# conclusion: this is not a good scale. in discussion, review the alpha of other studies that have used similar scales.

#######################################################################################
############################# Scale name ##############################################
#######################################################################################

#Los últimos tres meses, he tomado medidas para apoyar a mi joven a...
# 23.	… desarrollar un plan para estudiar y hacer la tarea
# 24.	… encontrar formas de involucrarse en las actividades escolares
# 25.	… encontrar maneras de involucrarse en actividades de voluntario/a en la comunidad
# 26.	… hacer metas para el año escolar
# 27.	… cumplir fechas de limite
# 28.	… participar en papeles de liderazgo en la escuela o comunidad
# 29.	… enfocarse en hacer lo mejor posible
# 30.	… desarrollar un horario para completar la tarea y los proyectos escolares a tiempo

# The scale to factor analyze
struct <- parent_w1 %>%
  select(q23, q24, q25, q26, q27, q28, q29, q30) 

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(struct) # 0.76

# Poly corr matrix
poly <- polychoric(struct)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # healthy corrs.

# scree plot
scree(struct, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 1 factor solution

# parallel test, anyways
fa.parallel(struct, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  4

# EFA using ULS 
factor_test_uls <- fa(struct, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# no warnings. all loading above .30

# EFA using ULS 
factor_test_uls <- fa(struct, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 
# warnings. Too many crossloadings. one heywood case.


# alpha
alpha(struct) # 0.82


#######################################################################################
########################### Trust (prev, Problem solving) #############################
#######################################################################################

# 122.	Cuando mi joven tiene que tomar decisiones importantes o cuando él / ella enfrenta un desafío, él / ella me pide por ayuda y apoyo____
# 123.	Mi joven y yo podemos hablar abiertamente de situaciones y temas difíciles (como sexo, drogas, la influencia de amigos y compañeros, etc.) ____
# 124.	Mi joven y yo podemos hablar y resolver conflictos que ocurren en casa ____
# 125.	Mi joven y yo podemos hablar y resolver los conflictos que surgen acerca de hacer la tarea____
# 126.	Si mi joven tiene problemas en la escuela, sé cómo conseguirle la ayuda que él / ella necesita___
# 127.	Si mi joven tiene un problema en la escuela, estoy segura/o de que él / ella podría hablar conmigo al respecto ___


# The scale to factor analyze
trust <- parent_w1 %>%
  select(q122, q123, q124, q125, q126, q127) 

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(trust) # 0.76 --> ok

# Poly corr matrix
poly <- polychoric(trust)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # look good

# scree plot
scree(trust, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 1 factor solution

# parallel test, anyways
fa.parallel(trust, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  1. Warnings

# EFA using ULS 
factor_test_uls <- fa(trust, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# warnings. ultra heywood case.
# item 124 loads 101.1

# removing item 124
trust <- parent_w1 %>%
  select(q122, q123, q125, q126, q127) 

# EFA using ULS 
factor_test_uls <- fa(trust, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# no warnings

ULS1   h2   u2 com
q122 0.84 0.71 0.29   1
q123 0.78 0.60 0.40   1
q125 0.76 0.58 0.42   1
q126 0.67 0.45 0.55   1
q127 0.85 0.73 0.27   1

# alpha
alpha(trust) # .81

# student trust in parent.


######################################################################################################
############################# convos ##############################################
######################################################################################################

# en los últimos tres meses, usted ha tenido una conversación con su joven sobre…
# 68.	… sus amistades en la escuela. ___ 
# 69.	… su participación en actividades escolares. ____
# 70.	… eventos que van a pasar en la escuela. ___ 
# 71.	… sus profesores. ___
# 72.	… las cosas que aprende en clase. ___
# 73.	… cómo contribuye al salón de clase. ___
# 74.	… cómo va en sus clases. ___
# 75.	… sus futuros objetivos de carrera y educación (incluso los objetivos a corto plazo). ____
# 76.	… desafíos que surgen en la escuela (como intimidación, agresión, acoso, racismo, peleas, etc.) ___
# 77.	… sus actitudes sobre la escuela ____
# 78.	… su comportamiento en la escuela ____
# 79.	… las cosas que suceden en la escuela (el ambiente escolar en general) ____
# 80.	… la importancia de prepararse para futuras trayectorias educativas o de carrera, como la planificación universitaria y / o pensando acerca de la escuela técnica o comercial, etc. _____
# 81.	… su involucro en actividades extracurriculares en la escuela y en la comunidad como en papeles de liderazgo, haciendo trabajo voluntario en la comunidad, deportes, etc._____


# The scale to factor analyze
convos <- parent_w1 %>%
  select(q68, q69, q70, q71, q72, q73, q74, q75, q76, q77, q78, q79, q80, q81) 

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(convos) # 0.86

# Poly corr matrix
poly <- polychoric(convos)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # looks good, lower corre among items 80, 81, 73

# scree plot
scree(convos, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot shows an aloost 2 factor solution, but the second factor is a little less than 1.0 eigenvalue

# parallel test, anyways
fa.parallel(convos, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  3
# warnings.

# EFA using ULS 
factor_test_uls <- fa(convos, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 3) 
# no warnings. 

     ULS1  ULS3  ULS2   h2   u2 com

q76  0.75 -0.01  0.09 0.62 0.38 1.0
q77  0.81  0.17 -0.02 0.85 0.15 1.1
q78  0.96 -0.05  0.01 0.87 0.13 1.0
q79  0.79  0.12 -0.01 0.75 0.25 1.0

q68  0.23  0.52  0.04 0.50 0.50 1.4     
q69  0.01  0.83  0.06 0.77 0.23 1.0
q70  0.03  0.84 -0.09 0.66 0.34 1.0
q71  0.04  0.77  0.05 0.68 0.32 1.0

q75  0.24  0.02  0.73 0.79 0.21 1.2
q80  0.14 -0.09  0.82 0.72 0.28 1.1
q81 -0.26  0.16  0.79 0.60 0.40 1.3

crossloading:

q72  0.10  0.36  0.41 0.54 0.46 2.1
q73  0.09  0.31  0.36 0.41 0.59 2.1
q74  0.49  0.14  0.41 0.78 0.22 2.1

# 72.	… las cosas que aprende en clase. ___
# 73.	… cómo contribuye al salón de clase. ___
# 74.	… cómo va en sus clases. ___

# removing crossloading items
convos <- parent_w1 %>%
  select(q68, q69, q70, q71, q75, q76, q77, q78, q79, q80, q81) 

# EFA using ULS 
factor_test_uls <- fa(convos, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 3) 
# no warnings. all loading above .30

      
# factor 1: school climate & behavior
     ULS3  ULS2   h2    u2 com
q76  0.71  0.03  0.11 0.62 0.385 1.1
q77  0.79  0.19  0.00 0.84 0.158 1.1
q78  0.99 -0.07  0.02 0.91 0.089 1.0
q79  0.78  0.13  0.01 0.75 0.250 1.1

# 76.	… desafíos que surgen en la escuela (como intimidación, agresión, acoso, racismo, peleas, etc.) ___
# 77.	… sus actitudes sobre la escuela ____
# 78.	… su comportamiento en la escuela ____
# 79.	… las cosas que suceden en la escuela (el ambiente escolar en general) ____

convos_climate <- parent_w1 %>%
  select(q76, q77, q78, q79) 
# alpha
alpha(convos_climate) # 0.85

# factor 2: socialization/social support
q68  0.17  0.58  0.04 0.52 0.481 1.2
q69 -0.03  0.89  0.07 0.83 0.174 1.0
q70  0.06  0.80 -0.09 0.63 0.367 1.0
q71  0.10  0.69  0.06 0.63 0.374 1.1

# 68.	… sus amistades en la escuela. ___ 
# 69.	… su participación en actividades escolares. ____
# 70.	… eventos que van a pasar en la escuela. ___ 
# 71.	… sus profesores. ___

convos_social <- parent_w1 %>%
  select(q68, q69, q70, q71) 
# alpha
alpha(convos_social) # 0.8

# factor 3: school climate & behavior
q75  0.18  0.10  0.71 0.75 0.247 1.2
q80  0.12 -0.09  0.87 0.79 0.214 1.1
q81 -0.23  0.17  0.74 0.55 0.448 1.3

# 75.	… sus futuros objetivos de carrera y educación (incluso los objetivos a corto plazo). ____
# 80.	… la importancia de prepararse para futuras trayectorias educativas o de carrera, como la planificación universitaria y / o pensando acerca de la escuela técnica o comercial, etc. _____
# 81.	… su involucro en actividades extracurriculares en la escuela y en la comunidad como en papeles de liderazgo, haciendo trabajo voluntario en la comunidad, deportes, etc._____

convos_college <- parent_w1 %>%
  select(q75, q80, q81) 
# alpha
alpha(convos_college) # 0.77

# Omega 
omega(convos, nfactors = 3, n.obs = 95, flip = T, plot = T) # .86 looks really beautiful. 


#######################################################################################
######################### Parent future orientation ##########################################
#######################################################################################

# Para mi es importante…
# 
# 105.	… que mi joven se gradúe de la escuela secundaria. _____
# 106.	… que mi joven continúe su educación después de la secundaria_____
# 107.	… saber cuáles son las metas que tiene mi joven para su futuro _____
# 108.	… ayudar a mi joven a hacer planes y dar pasos hacia sus metas para el futuro_____
# 109.	… buscar información para ayudarle a mi joven a alcanzar sus metas futuras _____
# 110.	… ayudar a mi joven a comenzar a prepararse para la educación superior, incluso mientras esté en la escuela intermedia____


# The scale to factor analyze
fut_or <- parent_w1 %>%
  select(q105, q106, q107, q108, q109, q110) 

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(fut_or) # 0.86

# Poly corr matrix
poly <- polychoric(fut_or)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # looks very blue, not sure if there will be collinearity?

# scree plot
scree(fut_or, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 1 factor solution

# parallel test, anyways
fa.parallel(fut_or, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE) 
# Parallel analysis suggests that the number of factors =  1, there are warnings, heywood case

# EFA using ULS 
factor_test_uls <- fa(fut_or, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# warning: matrix non-positive def
# item 106 0.99

fut_or <- parent_w1 %>%
  select(q105, q107, q108, q109, q110) 

# EFA without 106
factor_test_uls <- fa(fut_or, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) # no warnings

     ULS1   h2    u2 com
q105 0.84 0.71 0.289   1
q107 0.95 0.91 0.093   1
q108 0.99 0.98 0.024   1
q109 0.98 0.95 0.046   1
q110 0.72 0.52 0.476   1

# alpha
alpha(fut_or) # 0.89


#######################################################################################
############################# school involvement & others ##############################################
#######################################################################################

# En general, hago un esfuerzo para...
# 31.	… conocer el personal y la administración de la escuela  ___ 
# 32.	… conocer al menos uno de los maestros de mi joven ____ 
# 33.	… entender las reglas y pólizas de la escuela  ____ 
# 34.	… informarme sobre mis derechos como padre ____ 
# 35.	… aprender sobre el sistema educativo en este estado ____
# 36.	… entender la diferencia entre obtener un GED, graduarse con un diploma estándar de la escuela secundaria, o con un diploma de una secundaria internacional o con un diploma de Bachillerato Internacional.____ 
# 37.	… involucrarse en las actividades escolares, en el salón de clase, y/u otras maneras (por ejemplo, organizaciones de padres, trabajo voluntario, etc.) 
# 38.	… tener conversaciones con los otros padres para obtener información o aprender acerca de los recursos en la escuela. ____
# 39.	…contactar los otros padres para obtener apoyo. ____
# 40.	…entender la trayectoria hacia la preparación a la universidad y para una carrera ____
# 41.	… asistir a la conferencia de padres y maestros cuando esté disponible. _____ 


# The scale to factor analyze
scho_inv <- parent_w1 %>%
  select(q31, q32, q33, q34, q35, q36, q37, q38, q39, q40, q41)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(scho_inv) # 0.83

# Poly corr matrix
poly <- polychoric(scho_inv)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # item 41 with 37-39 neg corrs.

# scree plot
scree(scho_inv, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot  shows 2 factor solution

# parallel test, anyways
fa.parallel(scho_inv, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  3

# EFA using ULS 
factor_test_uls <- fa(scho_inv, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 
# warnings- try diff estimation methd
# items 35, 40, 41 crossload

     ULS1  ULS2   h2    u2 com
q31  0.58  0.14 0.42 0.580 1.1
q32  0.91 -0.07 0.78 0.224 1.0
q33  0.98 -0.03 0.94 0.057 1.0
q34  0.79  0.18 0.77 0.233 1.1

q36  0.27  0.55 0.50 0.498 1.5
q37 -0.07  0.86 0.69 0.313 1.0
q38  0.03  0.81 0.68 0.318 1.0
q39 -0.04  0.76 0.55 0.450 1.0

q35  0.63  0.30 0.64 0.359 1.4
q40  0.35  0.56 0.59 0.407 1.7
q41  0.61 -0.31 0.32 0.684 1.5

# without 35, 40, 41
scho_inv <- parent_w1 %>%
  select(q31, q32, q33, q34, q36, q37, q38, q39)

factor_test_uls <- fa(scho_inv, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 
# no warning

     ULS1  ULS2   h2    u2 com

# 31.	… conocer el personal y la administración de la escuela  ___ 
# 32.	… conocer al menos uno de los maestros de mi joven ____ 
# 33.	… entender las reglas y pólizas de la escuela  ____ 
# 34.	… informarme sobre mis derechos como padre ____ 

# Factor 1: knowing school personnel and rules
q31  0.60  0.17 0.47 0.533 1.2
q32  1.00 -0.10 0.93 0.071 1.0
q33  0.96  0.00 0.93 0.071 1.0
q34  0.75  0.19 0.70 0.300 1.1

# 36.	… entender la diferencia entre obtener un GED, graduarse con un diploma estándar de la escuela secundaria, o con un diploma de una secundaria internacional o con un diploma de Bachillerato Internacional.____ 
# 37.	… involucrarse en las actividades escolares, en el salón de clase, y/u otras maneras (por ejemplo, organizaciones de padres, trabajo voluntario, etc.) 
# 38.	… tener conversaciones con los otros padres para obtener información o aprender acerca de los recursos en la escuela. ____
# 39.	…contactar los otros padres para obtener apoyo. ____

# Factor 2: networking and...
q36  0.26  0.50 0.43 0.575 1.5
q37 -0.06  0.84 0.67 0.327 1.0
q38  0.05  0.82 0.71 0.285 1.0
q39 -0.02  0.76 0.57 0.431 1.0


scho_inv_fa1 <- parent_w1 %>%
  select(q31, q32, q33, q34)

# alpha
alpha(scho_inv_fa1) # 0.85

scho_inv_fa2 <- parent_w1 %>%
  select(q36, q37, q38, q39)

# alpha
alpha(scho_inv_fa2) # .78 (efforts to know school rules & personnel)

# removing 36 because it doesn't jive as well with othe ritems conceptually
scho_inv_fa2 <- parent_w1 %>%
  select(q37, q38, q39)

# alpha
alpha(scho_inv_fa2) # .79 (networking with other parents)

scho_inv <- parent_w1 %>%
  select(q31, q32, q33, q34, q37, q38, q39)

# Omega 
omega(scho_inv, nfactors = 2, n.obs = 95, flip = T, plot = T) # alpha 0.82
# warning : 3 factors are required fo identification


#######################################################################################
############################# self-efficacy ##############################################
#######################################################################################

# En general, estoy segura/o de que…
# 42.	…yo entiendo cómo funciona la escuela de mi joven. ____
# 43.	…puedo comunicar mis preguntas y preocupaciones con los maestros y el personal de la escuela.___
# 44.	…puedo trabajar con la escuela para encontrar una solución positiva si surge un conflicto o un problema que involucre a mi joven en la escuela. ____
# 45.	…estoy ayudando a mi joven hacer bien en la escuela. ____
# 46.	… estoy ayudando a mi joven prepararse para sus futuras metas de educación y carrera. ____


# The scale to factor analyze
s_efficacy <- parent_w1 %>%
  select(q42, q43, q44, q45, q46)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(s_efficacy) # 0.71

# Poly corr matrix
poly <- polychoric(s_efficacy)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # all healthy corrs

# scree plot
scree(s_efficacy, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 1 factor solution

# parallel test, anyways
fa.parallel(s_efficacy, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  3
# warnings

# EFA using ULS 
factor_test_uls <- fa(s_efficacy, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# no warnings

    ULS1   h2   u2 com
q42 0.88 0.78 0.22   1
q43 0.81 0.65 0.35   1
q44 0.81 0.65 0.35   1
q45 0.64 0.40 0.60   1
q46 0.66 0.44 0.56   1

# alpha
alpha(s_efficacy) # 0.8 (parent self-efficacy working with school & supporting child)

#######################################################################################
############################# comfort ##############################################
#######################################################################################

# Como madre/padre de esta escuela, siento que soy…
# 
# 47.	…parte de una comunidad con el personal de la escuela y los otros padres. ____ 
# 48.	…tratado/a con respeto, sabiendo que mis opiniones son importantes. ____
# 49.	…cómoda/o preguntando por un traductor y por materiales en español si los necesito. ____ 
# 50.	… capaz de hablar con maestros o administradores sobre grandes preocupaciones relacionados con mi joven. ___ 
# 51.	…dedicada/o en creando un ambiente exitoso para todos los jóvenes. ____
# 52.	 …feliz de que mi joven asista a esta escuela. ___
# 53.	…bienvenido/a en la escuela de mi joven. ____

# taps on parent comfort at school, except maybe item 51 about parent contributing 

# The scale to factor analyze
comfort <- parent_w1 %>%
  select(q47, q48, q49, q50, q51, q52, q53)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(comfort) # 0.83

# Poly corr matrix
poly <- polychoric(comfort)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # all healthy corrs

# scree plot
scree(comfort, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 1 factor solution

# parallel test, anyways
fa.parallel(comfort, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  3
# warnings

# EFA using ULS 
factor_test_uls <- fa(comfort, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 
# no warnings
#going for 2, because at least a factor would be 2 items

     ULS1  ULS2   h2    u2 com
# 48.	…tratado/a con respeto, sabiendo que mis opiniones son importantes. ____
# 49.	…cómoda/o preguntando por un traductor y por materiales en español si los necesito. ____ 
# 52.	 …feliz de que mi joven asista a esta escuela. ___
# 53.	…bienvenido/a en la escuela de mi joven. ____

q48  0.84 -0.04 0.67 0.330 1.0
q49  0.75  0.24 0.83 0.172 1.2
q52  0.73  0.24 0.78 0.216 1.2
q53  0.98 -0.12 0.85 0.150 1.0

comfort_fa1 <- parent_w1 %>%
  select(q48, q49, q52, q53)
# alpha
alpha(comfort_fa1) # 0.86

# 47.	…parte de una comunidad con el personal de la escuela y los otros padres. ____ 
# 50.	… capaz de hablar con maestros o administradores sobre grandes preocupaciones relacionados con mi joven. ___ 
# 51.	…dedicada/o en creando un ambiente exitoso para todos los jóvenes. ____

q47  0.13  0.44 0.27 0.732 1.2
q50  0.19  0.83 0.91 0.087 1.1
q51 -0.09  0.95 0.81 0.191 1.0

comfort_fa2 <- parent_w1 %>%
  select(q47, q50, q51)
# alpha
alpha(comfort_fa2) # 0.72

# Omega 
omega(comfort, nfactors = 2, n.obs = 95, flip = T, plot = T) # 0.84


#######################################################################################
############################# endorse & rels ##############################################
#######################################################################################

# Como madre/padre en esta escuela, estoy segura/o de que…	
# 54.	… esta escuela es un buen lugar para mi joven. _____  
# 55.	… el personal de la escuela de mi joven está haciendo cosas buenas por ella / el. _____  
# 56.	… la gente en la escuela de mi joven es confiable. _____  
# 57.	… la escuela de mi joven hace un buen trabajo preparando a los jóvenes para sus futuros. _____ 
# 58.	… puedo encontrar ayuda para mi joven si él / ella está batallando en una clase. ___
# 59.	… los maestros de mi joven se preocupan por ella/el. ____
# 60.	… maestros y administradores trabajan juntos para crear un ambiente seguro y acogedor para todos. ____
# 61.	… hay muchas oportunidades para la involucración de los padres. ____
# 62.	… puedo tener una conversación honesta y respetuosa sobre mi joven con su maestro. ___
# 63.	…puedo trabajar con un maestro para resolver cualquier problema que tenga mi joven en la escuela. _____


# The scale to factor analyze
rels <- parent_w1 %>%
  select(q54, q55, q56, q57, q58, q59, q60, q61, q62, q63)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(rels) # 0.88

# Poly corr matrix
poly <- polychoric(rels)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # all healthy corrs

# scree plot
scree(rels, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 1 factor solution

# parallel test, anyways
fa.parallel(rels, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  2
# warnings

# EFA using ULS 
factor_test_uls <- fa(rels, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 
# matrix nonpositive definite

# trying only a couple of items that tap more closely on endorsemnet

# 54.	… esta escuela es un buen lugar para mi joven. _____  
# 55.	… el personal de la escuela de mi joven está haciendo cosas buenas por ella / el. _____  
# 56.	… la gente en la escuela de mi joven es confiable. _____  
# 57.	… la escuela de mi joven hace un buen trabajo preparando a los jóvenes para sus futuros. _____ 

endorse <- parent_w1 %>%
  select(q54, q55, q56, q57)

factor_test_uls <- fa(endorse, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# warnings: ultra heywood case

    ULS1   h2      u2 com
q54 0.65 0.42  0.5776   1
q55 1.00 1.00 -0.0032   1
q56 0.96 0.93  0.0740   1
q57 0.97 0.94  0.0572   1

# removing 55 bec was redundant with 57
endorse <- parent_w1 %>%
  select(q54, q56, q57)

factor_test_uls <- fa(endorse, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# no warnings

    ULS1  h2     u2 com
q54 0.63 0.4 0.6030   1
q56 0.95 0.9 0.0961   1
q57 1.00 1.0 0.0041   1

# alpha
alpha(endorse) # 0.81


#######################################################################################
############################# support ##############################################
#######################################################################################

# 58.	… puedo encontrar ayuda para mi joven si él / ella está batallando en una clase. ___
# 59.	… los maestros de mi joven se preocupan por ella/el. ____
# 60.	… maestros y administradores trabajan juntos para crear un ambiente seguro y acogedor para todos. ____
# 61.	… hay muchas oportunidades para la involucración de los padres. ____
# 62.	… puedo tener una conversación honesta y respetuosa sobre mi joven con su maestro. ___
# 63.	…puedo trabajar con un maestro para resolver cualquier problema que tenga mi joven en la escuela. _____


# The scale to factor analyze
support <- parent_w1 %>%
  select(q58, q59, q60, q61, q62, q63)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(support) # 0.81

# Poly corr matrix
poly <- polychoric(support)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # all healthy corrs

# scree plot
scree(support, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot shows 1 factor solution

# parallel test, anyways
fa.parallel(support, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  2
# warnings

# EFA using ULS 
factor_test_uls <- fa(support, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 
# matrix nonpositive definite

# EFA using ULS 
factor_test_uls <- fa(support, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# matrix nonpositive definite


support <- parent_w1 %>%
  select(q58, q59, q62, q63)

# EFA using ULS 
factor_test_uls <- fa(support, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# no warnings!

    ULS1   h2    u2 com
q58 0.98 0.95 0.047   1
q59 0.92 0.85 0.153   1
q62 0.86 0.74 0.260   1
q63 0.85 0.73 0.272   1

# 58.	… puedo encontrar ayuda para mi joven si él / ella está batallando en una clase. ___
# 59.	… los maestros de mi joven se preocupan por ella/el. ____
# 62.	… puedo tener una conversación honesta y respetuosa sobre mi joven con su maestro. ___
# 63.	…puedo trabajar con un maestro para resolver cualquier problema que tenga mi joven en la escuela. _____


# conclusion: there appears to be something wrong with items 60 and/or 61

# alpha
alpha(support) # 0.88 (teacher support)


#######################################################################################
############################# p-t rel ##############################################
#######################################################################################

# The scale to factor analyze
pt_rel <- parent_w1 %>%
  select(q64, q65, q66, q67)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(pt_rel) # 0.77

# Poly corr matrix
poly <- polychoric(pt_rel)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # very high corrs

# scree plot
scree(pt_rel, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 1 factor solution

# parallel test, anyways
fa.parallel(pt_rel, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  1
# warnings


#warnings: ultraheywood

ULS1   h2     u2 com
q64 0.79 0.63  0.370   1
q65 0.88 0.78  0.225   1
q66 1.01 1.01 -0.011   1
q67 0.99 0.98  0.017   1

pt_rel <- parent_w1 %>%
  select(q64, q65, q67)

# without 66
factor_test_uls <- fa(pt_rel, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 

ULS1   h2      u2 com
q64 0.76 0.57  0.4290   1
q65 0.90 0.81  0.1861   1
q67 1.00 1.00 -0.0028   1

# scale doesn't work. Makes no sense to have a scale with only 2 items. 


### 
### 
### 
### 
### 
### 
### 
### 

### CODE TO COPY PASTE ###

#######################################################################################
############################# Scale name ##############################################
#######################################################################################

# The scale to factor analyze
scale_name <- datafile_name %>%
  select(item1, item2, item3, item...n)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(scale_name) # 

# Poly corr matrix
poly <- polychoric(scale_name)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F)

# scree plot
scree(scale_name, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows X factor solution

# parallel test, anyways
fa.parallel(scale_name, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  X

# EFA using ULS 
factor_test_uls <- fa(scale_name, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 

# alpha
alpha(scale_name) # X

# Omega 
omega(scale_name, nfactors = 1, n.obs = 95, flip = T, plot = T) # likes more than 1 factor, but still good to know Omega Total X (same as alpha)


#######################################################################################
#######################################################################################
#######################################################################################






