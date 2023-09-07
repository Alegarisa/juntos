
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
factor_test_uls <- fa(posi_rel_2, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) # no warning message :)

fa.diagram(factor_test_uls) # item 119 has a factor loading of 1

# EFA using pa 
factor_test_pa <- fa(posi_rel_2, n.obs = 95, rotate = "oblimin", fm = "pa", cor = "poly", nfactors = 1) # no warning message :)

fa.diagram(factor_test_pa) # item 119 has a factor loading of 1

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

############ CFA ##############

# el siguiente paso seria usar lavaan, especificar el modelo 
rp <- ("rp=~ q116 + q117 + q118 + q119 + q121") # aqui necesito un long format (116 seria variable y row seria tiempo 1, 2 y 3)


# [TBD]

#######################################################################################
############################# positive involvement ####################################
#######################################################################################

# this scale is dichotomous

### The items ###
# 1.	Trabajamos en un pasatiempo o artesanía.
# 2.	Participamos en una actividad al aire libre.
# 3.	Leímos o hablamos acerca de un libro o historia.
# 4.	Fuimos a un evento de entretenimiento.
# 5.	Participamos en otras actividades (Fuimos al parque, nadamos, excursión a pie, etc.). 
# 6.	Horneamos o cocinamos una comida.
# 7.	Hicimos ejercicio o jugamos un juego al aire libre (baloncesto o béisbol, etc.)
# 8.	Trabajamos alrededor de la casa o patio.
# 9.	Fuimos a la Iglesia, sinagoga, u otro servicio religioso.

# leaving 8 and 9 out because seem out of concept #kmo with was 0.6

# The scale to factor analyze
posi_inv <- parent_w1 %>%
  select(q1, q2, q3, q4, q5, q6, q7)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(posi_inv) # 0.56 (ver 1 = pretty poor)

# Poly corr matrix
poly <- tetrachoric(posi_inv)
rho <- poly$rho
cor.plot(rho, numbers = T, upper = F, main = "Tetrachoric", show.legend = F) # doesn't look great. 


fa.parallel(rho, n.obs=95, fm="uls", fa="both", main="Parallel Analysis Scree Plots", cor="tet", use="pairwise", plot=TRUE) # says 2 factors, but kmo is not good enough; 

# EFA using ULS 
factor_test_uls <- fa(rho, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "tet", nfactors = 1) 

# ULS1    h2   u2 com
q1 0.37 0.134 0.87   1
q2 0.63 0.403 0.60   1
q3 0.16 0.027 0.97   1
q4 0.19 0.038 0.96   1
q5 0.70 0.490 0.51   1
q6 0.25 0.061 0.94   1
q7 0.79 0.621 0.38   1

posi_inv <- parent_w1 %>%
  select(q1, q2, q5, q7)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(posi_inv) # 0.64 better

# EFA using ULS 
factor_test_uls <- fa(posi_inv, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "tet", nfactors = 1)
# no warnings

#  ULS1    h2   u2 com
q1 0.31 0.095 0.90   1
q2 0.62 0.382 0.62   1
q5 0.71 0.507 0.49   1
q7 0.83 0.694 0.31   1

# Omega 
omega(posi_inv, nfactors = 1, n.obs = 95, flip = T, plot = T) # 0.57

# Conclusion: evidence of construct validity, but it has poor internal consistency (relation between items is not very high).  

# make a CFA using wave 2 and 3, to check time invariance. [TBD]


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
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # looks good

# parallel test, anyways
fa.parallel(monit, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  2, warnings about ulta-heywood case

# EFA using ULS 
factor_test_uls <- fa(monit, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# No warnings
# all factor loadings above 0.4

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

monit <- parent_w1 %>%
  select(q138, q139, q140, q141, q142, q143, q144)

# Omega
omega(monit, nfactors = 2, n.obs = 95, flip = T, plot = T) # likes more than 1 factor, but still good to know Omega Total 0.82 (same as alpha)


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
  select(q84, q85, q86, q87, q88, q89, q90, q91, q92, q93, q94, q95, q96, q97, q98)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(hw_inv) # 0.71 --> ok

# Poly corr matrix
poly <- polychoric(hw_inv)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F)
# items 84, 89, 92 neg corr with other items

# EFA using ULS 
factor_test_uls <- fa(hw_inv, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# items 91 and 92 load at or below .31 

# new efa iteration without problematic items
hw_inv_2 <- parent_w1 %>%
  select(q84, q85, q86, q87, q88, q89, q90, q93, q94, q95, q96, q97, q98)

# Poly corr matrix
poly <- polychoric(hw_inv_2)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # looks better

# EFA using ULS 
factor_test_uls <- fa(hw_inv_2, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 

# scree plot
scree(hw_inv_2, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 2 factor solution

# parallel test, anyways
fa.parallel(hw_inv_2, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  3

# EFA using ULS - More than 1 factors 
factor_test_uls <- fa(hw_inv_2, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 

#factor 1
q86  0.59  0.13 0.39 0.61 1.1
q87  0.74 -0.08 0.53 0.47 1.0
q88  0.84 -0.18 0.69 0.31 1.1
q89  0.66  0.19 0.51 0.49 1.2
q90  0.39  0.23 0.23 0.77 1.6
q94  0.57  0.06 0.34 0.66 1.0
q96  0.51  0.09 0.29 0.71 1.1
q97  0.48  0.15 0.28 0.72 1.2

86	Me senté con mi joven mientras él / ella hacía su tarea
87	Me hice disponible para mi joven cuando tenía preguntas sobre su tarea
88	Ayudé a mi joven cuando estaba atascado/a en una tarea
89	Ayudé a mi joven a prepararse para los exámenes haciéndole preguntas sobre el tema, etc
90	Le di a mi joven incentivos como privilegios especiales, regalos especiales, etc.
94	Comprobé y corregí la tarea de mi hija/o 
96	Le ayudé a mi joven acceder a otros recursos (biblioteca, materiales de arte, laboratorio de computación, etc.)
97	Le ayudé a mi joven practicar habilidades (lectura, revisión, problemas de práctica, etc.)

# factor 2
q84 -0.11  0.91 0.81 0.19 1.0
q85  0.01  0.90 0.81 0.19 1.0
q93  0.22  0.65 0.51 0.49 1.2

84	Me aseguré de que tenga un cierto tiempo para hacer la tarea
85	Me aseguré de que él / ella tenga un cierto lugar para hacer la tarea
93	Comprobé con mi joven para asegurarme de que él / ella hiciera su tarea

# load into both factors
q95  0.34  0.37 0.30 0.70 2.0
q98  0.30  0.32 0.23 0.77 2.0

# Omega 
omega(hw_inv_2, nfactors = 2, n.obs = 95, flip = T, plot = T) # likes more than 1 factor, but still good to know Omega Total X (same as alpha)


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

limits <- parent_w1 %>%
  select(q128, q129, q130, q131, q132, q133, q134, q135, q136) 

# EFA using ULS 
factor_test_uls <- fa(limits, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 

limits <- parent_w1 %>%
  select(q128, q129, q130, q131, q132, q133, q134, q135) # removed 136 was neg corr with rest of items. Prob. neg wording. 

# Poly corr matrix
poly <- polychoric(limits)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(limits) # 0.8

fa.parallel(poly$rho, n.obs=95, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE) 
# Parallel analysis suggests that the number of factors =  4, but don't reach eigenvalue of 1
# shows warning messages: "The estimated weights for the factor scores are probably incorrect." and "An ultra-Heywood case was detected"

# EFA using ULS -- 2 factor
factor_test_uls <- fa(limits, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 
# warning messages

#     ULS1  ULS2   h2      u2 com
q128 0.78  0.27 0.91  0.0919 1.2
q129 0.54  0.45 0.77  0.2307 1.9
q130 0.07  0.96 1.00 -0.0025 1.0 #
q131 0.48 -0.19 0.17  0.8327 1.3
q132 0.74 -0.23 0.41  0.5888 1.2
q133 0.11  0.05 0.02  0.9798 1.5 #
q134 0.77 -0.21 0.46  0.5431 1.2 
q135 0.85  0.20 0.96  0.0425 1.1

# EFA using ULS -- 1 factor
factor_test_uls <- fa(limits, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# warning messages

#    ULS1    h2    u2 com
q128 0.97 0.94 0.059   1
q129 0.87 0.76 0.244   1
q130 0.71 0.50 0.500   1
q131 0.32 0.10 0.900   1
q132 0.53 0.28 0.721   1
q133 0.14 0.02 0.980   1
q134 0.57 0.33 0.674   1
q135 0.99 0.97 0.026   1

# conclusion: this is a 1-factor scale. One of the factors would have 2 items only

limits <- parent_w1 %>%
  select(q128, q129, q130, q131, q132, q133, q134) # removed 135 very high communality

# EFA using ULS
factor_test_uls <- fa(limits, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# warning messages

#   ULS1    h2     u2 com
q128 1.01 1.011 -0.011   1
q129 0.90 0.801  0.199   1
q130 0.69 0.476  0.524   1
q131 0.33 0.108  0.892   1
q132 0.49 0.241  0.759   1
q133 0.15 0.022  0.978   1
q134 0.53 0.286  0.714   1

limits <- parent_w1 %>%
  select(q129, q130, q131, q132, q133, q134) # removing 128 very high communality

factor_test_uls <- fa(limits, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# no warning messages

#    ULS1    h2    u2 com
q129 0.96 0.928 0.072   1
q130 0.66 0.430 0.570   1
q131 0.32 0.099 0.901   1
q132 0.49 0.239 0.761   1
q133 0.21 0.045 0.955   1
q134 0.48 0.230 0.770   1

# 129.	Mi joven sabe cómo voy a responder cuando hace algo malo cosas que no me gustan o lo que está en contra las reglas de la casa)
# 130.	Cada vez que mi joven hace algo mal, yo le respondo con una consecuencia específica (p. ej., una disciplina específica, quitándole privilegios, etc.) 
# 131.	Cuando mi joven hace algo mal, le grito o le insulto
# 132.	Puedo controlar mi enojo y mantenerme calmado/a cuando disciplino o discuto con mi joven cuando él / ella hace algo mal
# 133.	Cuando mi joven me desafía al no hacer lo que le pido, yo renuncio
# 134.	Cuando mi joven está aprendiendo un nuevo comportamiento (p. ej.: ser más responsable, estudioso/a u organizado/a), reconozco su progreso con, por ejemplo, un abrazo, una sonrisa o un pequeño regalo

limits <- parent_w1 %>%
  select(q129, q130, q131, q132, q134) # removing 133 loads less than 0.30

factor_test_uls <- fa(limits, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1)
# warnings

#    ULS1    h2     u2 com
q129 1.00 0.997 0.0027   1
q130 0.64 0.415 0.5854   1
q131 0.29 0.082 0.9176   1
q132 0.47 0.221 0.7790   1
q134 0.49 0.241 0.7591   1

limits <- parent_w1 %>%
  select(q130, q131, q132, q134) # removing 129 very high communality

factor_test_uls <- fa(limits, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1)
# no warnings

#    ULS1   h2   u2 com
q130 0.32 0.10 0.90   1
q131 0.45 0.20 0.80   1
q132 0.61 0.38 0.62   1
q134 0.62 0.39 0.61   1

# Omega 
omega(limits, nfactors = 1, n.obs = 95, flip = T, plot = T) # 0.51


#######################################################################################
########################### Trust (prev, Problem solving) #############################
#######################################################################################

# The scale to factor analyze
probs_sol <- parent_w1 %>%
  select(q122, q123, q124, q125, q126, q127) 

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(probs_sol) # 0.76 --> ok

# Poly corr matrix
poly <- polychoric(probs_sol)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # look good

# scree plot
scree(probs_sol, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 1 factor solution

# parallel test, anyways
fa.parallel(probs_sol, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  2

# EFA using ULS -- 2 factor
factor_test_uls <- fa(probs_sol, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 
# warnings

#    ULS1  ULS2   h2      u2 com
q122 0.91 -0.03 0.79  0.2074 1.0
q123 0.90 -0.08 0.73  0.2745 1.0
q124 1.02  0.00 1.05 -0.0484 1.0
q125 0.48  0.39 0.59  0.4130 1.9
q127 0.71  0.20 0.71  0.2909 1.2

q126 0.00  1.00 1.01 -0.0086 1.0 # one factor with one item. 

# EFA using ULS -- 1 factor
factor_test_uls <- fa(probs_sol, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# warnings

#    ULS1   h2     u2 com
q122 0.86 0.75  0.252   1
q123 0.81 0.66  0.339   1
q124 1.01 1.03 -0.027   1
q125 0.75 0.56  0.443   1
q126 0.63 0.39  0.607   1
q127 0.85 0.73  0.272   1

# conclusion, the scale is a 1-factor. 

probs_sol <- parent_w1 %>%
  select(q122, q123, q125, q126, q127)

factor_test_uls <- fa(probs_sol, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# no warnings

#    ULS1   h2   u2 com
q122 0.84 0.71 0.29   1
q123 0.78 0.60 0.40   1
q125 0.76 0.58 0.42   1
q126 0.67 0.45 0.55   1
q127 0.85 0.73 0.27   1

# Omega 
omega(probs_sol, nfactors = 1, n.obs = 95, flip = T, plot = T) # likes more than 1 factor, but still good to know Omega Total X (same as alpha)



######################################################################################################
############################# College readiness convos ##############################################
######################################################################################################


# The scale to factor analyze
readiness <- parent_w1 %>%
  select(q72, q73, q74, q75, q80, q81) 

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(readiness) # 0.82

# Poly corr matrix
poly <- polychoric(readiness)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # looks pretty good

# scree plot
scree(readiness, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE) # Scree plot clearly shows 1 factor solution

# parallel test, anyways
fa.parallel(readiness, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  1, there are warnings, though

# EFA using ULS 
factor_test_uls <- fa(readiness, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) # no warnings

#   ULS1   h2   u2 com
q72 0.72 0.51 0.49   1
q73 0.61 0.37 0.63   1
q74 0.81 0.66 0.34   1
q75 0.90 0.81 0.19   1
q80 0.80 0.64 0.36   1
q81 0.68 0.46 0.54   1

# alpha
alpha(readiness) # 0.81

# Omega 
omega(readiness, nfactors = 1, n.obs = 95, flip = T, plot = T) # likes more than 1 factor, but still good to know Omega Total X (same as alpha)


#######################################################################################
######################### Future Orientation ##########################################
#######################################################################################

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
fa.parallel(fut_or, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE) # Parallel analysis suggests that the number of factors =  1, there are warnings, heywood case

# EFA using ULS 
factor_test_uls <- fa(fut_or, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) # warning: matrix non-positive def

#    ULS1   h2    u2 com
q105 0.88 0.77 0.230   1
q106 0.99 0.98 0.016   1 #
q107 0.95 0.90 0.098   1
q108 0.98 0.96 0.045   1
q109 0.96 0.92 0.080   1
q110 0.72 0.52 0.481   1

# although 106 is the one has highest factor load and h2, I want to check if taking 105, what happens
fut_or <- parent_w1 %>%
  select(q106, q107, q108, q109, q110) 

# parallel test, anyways
fa.parallel(fut_or, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE) # Parallel analysis suggests that the number of factors =  1, there are warnings, heywood case

# EFA using ULS 
factor_test_uls <- fa(fut_or, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) # no warnings

#    ULS1   h2    u2 com
q106 0.96 0.93 0.074   1
q107 0.95 0.91 0.092   1
q108 0.99 0.98 0.021   1 #
q109 0.96 0.93 0.069   1
q110 0.73 0.54 0.460   1

fut_or <- parent_w1 %>%
  select(q106, q107, q109, q110) # removing 108

# parallel test, anyways
fa.parallel(fut_or, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE) # Parallel analysis suggests that the number of factors =  1, there are warnings, heywood case

# EFA using ULS 
factor_test_uls <- fa(fut_or, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) # no warnings

#    ULS1   h2    u2 com
q106 0.96 0.93 0.070   1
q107 0.96 0.92 0.085   1
q109 0.96 0.92 0.083   1
q110 0.74 0.54 0.457   1

# alpha
alpha(fut_or) # 0.87

# Omega 
omega(fut_or, nfactors = 1, n.obs = 95, flip = T, plot = T) # likes more than 1 factor, but still good to know Omega Total X (same as alpha)


#######################################################################################
############################# Structure ##############################################
#######################################################################################

# The scale to factor analyze
struct <- parent_w1 %>%
  select(q23, q24, q25, q26, q27, q28, q29, q30) 

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(struct) # 0.76

# Poly corr matrix
poly <- polychoric(struct)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # looks good

# scree plot
scree(struct, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 1 factor solution

# parallel test, anyways
fa.parallel(struct, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  4, with warnings: heywood

# testing 3 factors because 4 would be each made of 2 items.
# EFA using ULS -- 3 factor
factor_test_uls <- fa(struct, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 3) # warning heywood

#    ULS1  ULS3  ULS2   h2      u2 com
q23  0.59  0.04  0.28 0.63  0.3744 1.4
q24  1.03 -0.01 -0.01 1.03 -0.0306 1.0 #
q25 -0.04  0.12  0.55 0.34  0.6570 1.1
q26  0.05  0.46  0.36 0.50  0.4962 1.9
q27  0.38  0.32  0.24 0.59  0.4147 2.7
q28  0.05 -0.01  0.97 1.00  0.0037 1.0
q29 -0.03  0.99  0.03 0.98  0.0224 1.0
q30  0.30  0.53 -0.24 0.44  0.5644 2.1

# factors don't make sense conceptually. 

# EFA using ULS -- 2 factor
factor_test_uls <- fa(struct, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) # warning

#   ULS1  ULS2   h2    u2 com
q23 0.46  0.42 0.56 0.435 2.0
q24 0.66  0.27 0.67 0.334 1.3
q25 0.10  0.50 0.31 0.691 1.1
q26 0.47  0.32 0.46 0.538 1.7
q27 0.60  0.30 0.61 0.391 1.5
q28 0.01  0.99 0.99 0.005 1.0
q29 0.80  0.01 0.65 0.353 1.0
q30 0.80 -0.27 0.52 0.483 1.2

# factors don't make sense conceptually. 

# EFA using ULS -- 1 factor
factor_test_uls <- fa(struct, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) # no warnings

#   ULS1   h2   u2 com
q23 0.76 0.58 0.42   1
q24 0.82 0.68 0.32   1
q25 0.48 0.23 0.77   1
q26 0.69 0.48 0.52   1
q27 0.79 0.63 0.37   1
q28 0.71 0.50 0.50   1
q29 0.72 0.52 0.48   1
q30 0.49 0.24 0.76   1

# alpha
alpha(struct) # 0.82

# Omega 
omega(struct, nfactors = 2, n.obs = 95, flip = T, plot = T) # likes more than 1 factor, but still good to know Omega Total X (same as alpha)


## HERE ##








### 
### 
### 
### 



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