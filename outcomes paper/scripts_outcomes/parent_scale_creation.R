
### The dataset ###
parent_w1 <- read_sav(here("outcomes paper/data_outcomes", "primary_parent_w1.sav"))

view_df(parent_w1)

### scales creation

parent_scales <- parent_w1 %>%
  rowwise() %>% 
  mutate(posirel_1 = mean(c(q116, q117, q118, q119, q121), na.rm = TRUE),
         monitpeers_1 = mean(c(q140, q141, q142, q143), na.rm = TRUE),
         acadhwi_1 = mean(c(q86, q87, q88, q89, q94, q96, q97), na.rm = TRUE),
         checkhwi_1 = mean(c(q84, q85, q93), na.rm = TRUE),
         struct_1 = mean(c(q23, q24, q25, q26, q27, q28, q29, q30), na.rm = TRUE),
         trust_1 = mean(c(q122, q123, q125, q126, q127), na.rm = TRUE),
         convclimate_1 = mean(c(q76, q77, q78, q79), na.rm = TRUE),
         convsocial_1 = mean(c(q68, q69, q70, q71), na.rm = TRUE),
         convfut_1 = mean(c(q75, q80, q81), na.rm = TRUE),
         futorient_1 = mean(c(q105, q107, q108, q109, q110), na.rm = TRUE),
         knowschool_1 = mean(c(q31, q32, q33, q34), na.rm = TRUE),
         parnets_1 = mean(c(q37, q38, q39), na.rm = TRUE),
         selfeffic_1 = mean(c(q42, q43, q44, q45, q46), na.rm = TRUE),
         welcome_1 = mean(c(q48, q49, q52, q53), na.rm = TRUE),
         included_1 = mean(c(q47, q50, q51), na.rm = TRUE),
         endorse_1 = mean(c(q54, q56, q57), na.rm = TRUE),
         supported_1 = mean(c(q58, q59, q62, q63), na.rm = TRUE))

### posirel ### alpha = 0.89
# 116.	Cuando mi joven me pide hablar o cuando necesita hablar conmigo, escucho atentamente.
# 117.	Sé escuchar atentamente, aun cuando no esté de acuerdo con lo que dice la otra persona.
# 118.	Con regularidad, mi joven y yo hacemos cosas juntos que ambos disfrutamos
# 119.	Mi joven y yo tenemos una relación cercana (decided to keep it, despite loading of 1.0 because there was no warning)
# 121.	Como madre/padre, es mi trabajo reconocer y apoyar las fortalezas de mi joven

### monitpeers_1 ### alpha = 0.77
# 140.	Conozco muy bien a los amigos de mi joven.
# 141.	Los amigos de mi joven tienen una buena influencia en su vida.
# 142.	Los amigos de mi joven se apoyan positivamente entre sí.
# 143.	Por lo general yo sé con quién está mi joven

### acadhwi_1 ### alpha = 0.78
# 86	Me senté con mi joven mientras él / ella hacía su tarea
# 87	Me hice disponible para mi joven cuando tenía preguntas sobre su tarea
# 88	Ayudé a mi joven cuando estaba atascado/a en una tarea
# 89	Ayudé a mi joven a prepararse para los exámenes haciéndole preguntas sobre el tema, etc
# 94	Comprobé y corregí la tarea de mi hija/o 
# 96	Le ayudé a mi joven acceder a otros recursos (biblioteca, materiales de arte, laboratorio de computación, etc.)
# 97	Le ayudé a mi joven practicar habilidades (lectura, revisión, problemas de práctica, etc.)

### checkhwi_1 ### alpha = 0.77
# 84	Me aseguré de que tenga un cierto tiempo para hacer la tarea
# 85	Me aseguré de que él / ella tenga un cierto lugar para hacer la tarea
# 93	Comprobé con mi joven para asegurarme de que él / ella hiciera su tarea

# ### struct_1 ### alpha = 0.82
#Los últimos tres meses, he tomado medidas para apoyar a mi joven a...
# 23.	… desarrollar un plan para estudiar y hacer la tarea
# 24.	… encontrar formas de involucrarse en las actividades escolares
# 25.	… encontrar maneras de involucrarse en actividades de voluntario/a en la comunidad
# 26.	… hacer metas para el año escolar
# 27.	… cumplir fechas de limite
# 28.	… participar en papeles de liderazgo en la escuela o comunidad
# 29.	… enfocarse en hacer lo mejor posible
# 30.	… desarrollar un horario para completar la tarea y los proyectos escolares a tiempo

# ### trust_1 ### alpha = 0.81 -- child trusting parent
# 122.	Cuando mi joven tiene que tomar decisiones importantes o cuando él / ella enfrenta un desafío, él / ella me pide por ayuda y apoyo____
# 123.	Mi joven y yo podemos hablar abiertamente de situaciones y temas difíciles (como sexo, drogas, la influencia de amigos y compañeros, etc.) ____
# 125.	Mi joven y yo podemos hablar y resolver los conflictos que surgen acerca de hacer la tarea____
# 126.	Si mi joven tiene problemas en la escuela, sé cómo conseguirle la ayuda que él / ella necesita___
# 127.	Si mi joven tiene un problema en la escuela, estoy segura/o de que él / ella podría hablar conmigo al respecto ___

# ### convclimate_1 ### alpha = 0.85 -- convos with child
# 76.	… desafíos que surgen en la escuela (como intimidación, agresión, acoso, racismo, peleas, etc.) ___
# 77.	… sus actitudes sobre la escuela ____
# 78.	… su comportamiento en la escuela ____
# 79.	… las cosas que suceden en la escuela (el ambiente escolar en general) ____

# ### convsocial_1 ### alpha = 0.8 -- convos with child
# 68.	… sus amistades en la escuela. ___ 
# 69.	… su participación en actividades escolares. ____
# 70.	… eventos que van a pasar en la escuela. ___ 
# 71.	… sus profesores. ___

# ### convfut_1 ### alpha = 0.77 -- convos with child
# 75.	… sus futuros objetivos de carrera y educación (incluso los objetivos a corto plazo). ____
# 80.	… la importancia de prepararse para futuras trayectorias educativas o de carrera, como la planificación universitaria y / o pensando acerca de la escuela técnica o comercial, etc. _____
# 81.	… su involucro en actividades extracurriculares en la escuela y en la comunidad como en papeles de liderazgo, haciendo trabajo voluntario en la comunidad, deportes, etc._____

# ### futorient_1 ### alpha = 0.8 -- parent orientation to future/expectations
# 105.	… que mi joven se gradúe de la escuela secundaria. _____
# 107.	… saber cuáles son las metas que tiene mi joven para su futuro _____
# 108.	… ayudar a mi joven a hacer planes y dar pasos hacia sus metas para el futuro_____
# 109.	… buscar información para ayudarle a mi joven a alcanzar sus metas futuras _____
# 110.	… ayudar a mi joven a comenzar a prepararse para la educación superior, incluso mientras esté en la escuela intermedia

# ### knowschool_1 ### alpha = 0.85 -- parent efforts to know school
# 31.	… conocer el personal y la administración de la escuela  ___ 
# 32.	… conocer al menos uno de los maestros de mi joven ____ 
# 33.	… entender las reglas y pólizas de la escuela  ____ 
# 34.	… informarme sobre mis derechos como padre ____ 

# ### parnets_1 ### alpha = 0.79 -- parent networking 
# 37.	… involucrarse en las actividades escolares, en el salón de clase, y/u otras maneras (por ejemplo, organizaciones de padres, trabajo voluntario, etc.) 
# 38.	… tener conversaciones con los otros padres para obtener información o aprender acerca de los recursos en la escuela. ____
# 39.	…contactar los otros padres para obtener apoyo. ____

# ### selfeffic_1 ### alpha = 0.80
# En general, estoy segura/o de que…
# 42.	…yo entiendo cómo funciona la escuela de mi joven. ____
# 43.	…puedo comunicar mis preguntas y preocupaciones con los maestros y el personal de la escuela.___
# 44.	…puedo trabajar con la escuela para encontrar una solución positiva si surge un conflicto o un problema que involucre a mi joven en la escuela. ____
# 45.	…estoy ayudando a mi joven hacer bien en la escuela. ____
# 46.	… estoy ayudando a mi joven prepararse para sus futuras metas de educación y carrera. ____

# ### welcome_1 ### alpha = 0.86
# 48.	…tratado/a con respeto, sabiendo que mis opiniones son importantes. ____
# 49.	…cómoda/o preguntando por un traductor y por materiales en español si los necesito. ____ 
# 52.	 …feliz de que mi joven asista a esta escuela. ___
# 53.	…bienvenido/a en la escuela de mi joven. ____

# ### included_1 ### alpha = 0.72
# 47.	…parte de una comunidad con el personal de la escuela y los otros padres. ____ 
# 50.	… capaz de hablar con maestros o administradores sobre grandes preocupaciones relacionados con mi joven. ___ 
# 51.	…dedicada/o en creando un ambiente exitoso para todos los jóvenes. ____

# ### endorse_1 ### alpha = 0.81
# 54.	… esta escuela es un buen lugar para mi joven. _____  
# 56.	… la gente en la escuela de mi joven es confiable. _____  
# 57.	… la escuela de mi joven hace un buen trabajo preparando a los jóvenes para sus futuros. _____ 

# ### supported_1 ### alpha = 0.88
# 58.	… puedo encontrar ayuda para mi joven si él / ella está batallando en una clase. ___
# 59.	… los maestros de mi joven se preocupan por ella/el. ____
# 62.	… puedo tener una conversación honesta y respetuosa sobre mi joven con su maestro. ___
# 63.	…puedo trabajar con un maestro para resolver cualquier problema que tenga mi joven en la escuela. _____



# from mdm scales
### renaming demo variables to serve as covariates
# d_w1_clean3 <- d_w1_clean2 %>%
#   rename(affiliation = intro001_1,
#          gender = demo001_1,
#          age = demo002_1,
#          rel_status = demo003_1,
#          ed_level = demo004_1,
#          income_enough = demo007_1,
#          food_insec = demo008_1,
#          number_children = demo015_1,
#          prim_caregiver = demo021_1)
# 
# w1_scales_final <- w1_scales %>%
#   select(record_id, condition, wave, affiliation, gender, age, rel_status, ed_level, income_enough, food_insec, number_children, prim_caregiver, internalizing_1, externalizing_1, hyperactivity_1, prosocial_1, coercion_1, encouragement_1, pcpositiverel_1, famrels_1, probsol_1, monitoring_1, famcom_1, emoexpres_1, schinvolv_1, relteach_1, endorse_1, parpeers_1, unsafeneigh_1)

# next step: choose covariates
# keep it simple, age seems irrelevant for parents, age is duplicative with student grade. But that is for models, maybe be more inclusive on covariates, just in case. 

# Parents’ educational level and comfort with English
# Youth’s gender and grade level

# look into diss analyses. I have code there for this, but need to check if datasets will align because I did more work on EFA after graduating. 

