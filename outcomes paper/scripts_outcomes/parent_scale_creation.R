




### scale creation


parent_scales <- parent_w1 %>%
  rowwise() %>% 
  mutate(posirel_1 = mean(c(q116, q117, q118, q119, q121), na.rm = TRUE),
         monitpeers_1 = mean(c(q140, q141, q142, q143), na.rm = TRUE),
         acadhwi_1 = mean(c(q86, q87, q88, q89, q94, q96, q97), na.rm = TRUE),
         checkhwi_1 = mean(c(q84, q85, q93), na.rm = TRUE),
         coercion_1 = mean(c(pafas005_1, pafas007_1, pafas009_1, pafas010_1, pafas013_1), na.rm = TRUE),
         encouragement_1 = mean(c(pafas002_1, pafas006_1, pafas008_1), na.rm = TRUE),
         pcpositiverel_1 = mean(c(pafas014_1, pafas015_1, pafas016_1, pafas017_1, pafas018_1), na.rm = TRUE),
         famrels_1 = mean(c(pafas019_1, pafas020_1, pafas021_1, pafas022_1), na.rm = TRUE),
         probsol_1 = mean(c(coremdm016_1, coremdm017_1, coremdm018_1, coremdm019_1), na.rm = TRUE),
         monitoring_1 = mean(c(coremdm020_1, coremdm021_1, coremdm022_1, coremdm023_1, coremdm024_1), na.rm = TRUE),
         famcom_1 = mean(c(comfam001_1, comfam002_1, comfam005_1), na.rm = TRUE),
         emoexpres_1 = mean(c(comfam003_1, comfam004_1, comfam006_1), na.rm = TRUE),
         schinvolv_1 = mean(c(involveduc001_1, involveduc002_1, involveduc003_1, involveduc004_1, involveduc005_1), na.rm = TRUE),
         relteach_1 = mean(c(relteach001_1, relteach002_1, relteach003_1, relteach005_1, relteach006_1), na.rm = TRUE),
         endorse_1 = mean(c(endorschool001_1, endorschool002_1, endorschool003_1, endorschool004_1), na.rm = TRUE),
         parpeers_1 = mean(c(relpar001_1, relpar002_1, relpar003_1, relpar004_1, relpar005_1, relpar006_1), na.rm = TRUE),
         unsafeneigh_1 = mean(c(safeneigh001_1, safeneigh002_1, safeneigh003_1, safeneigh006_1), na.rm = TRUE))




### posirel ### alpha = 0.84
# 116.	Cuando mi joven me pide hablar o cuando necesita hablar conmigo, escucho atentamente.
# 117.	Sé escuchar atentamente, aun cuando no esté de acuerdo con lo que dice la otra persona.
# 118.	Con regularidad, mi joven y yo hacemos cosas juntos que ambos disfrutamos
# 119.	Mi joven y yo tenemos una relación cercana (decided to keep tghis one, despite loading of 1.0 because there was no warning)
# 121.	Como madre/padre, es mi trabajo reconocer y apoyar las fortalezas de mi joven

### monitpeers_1 ### alpha = 0.76
# 140.	Conozco muy bien a los amigos de mi joven. 
# 141.	Los amigos de mi joven tienen una buena influencia en su vida. 
# 142.	Los amigos de mi joven se apoyan positivamente entre sí.
# 143.	Por lo general yo sé con quién está mi joven
# 144.	Sé lo que hace mi joven y dónde va cuando no está en casa

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

