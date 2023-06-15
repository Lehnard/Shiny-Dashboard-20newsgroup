tratayPredice <- function(texto){

#########################################################################################                         
##                                                                                      #
##  Autores: Maria Calvo             Fecha: 22/05/2017                                  #
##           David Grinan                                                               #
##           Jordi Aceiton                                                              #
##                                                                                      #        
##                                                                                      #
##  tratayPredice(texto)                                                                #        
##                                                                                      #
##      Script que toma un texto (plain) con el objetivo de transformarlo al espacio    #
##      de la matriz TF-IDF para as? poder hacer prediccion de clasificacion con ?l.    #
##                                                                                      #
##      Luego el texto se classifica seg?n el modelo entrenado seleccionado que puede   #
##      encontrarse en "./modelos/" de tipo svm o logistic one vs all.                  #        
##                                                                                      #
##  input:                                                                              #
##                                                                                      #
##      texto --  Texto plano. p.ej. *.txt                                              #
##                                                                                      #
##  output:                                                                             #
##                                                                                      #
##      tratayPredice -- lista de 3 componentes que contiene:                           #
##                              - grupo: el grupo al que se clasifica 'texto'           # 
##                              - prob: probabilidades de pertenencia de 'texto a las   #
##                                      distintas clases.                               #
##                              - grupos: las distintas clases de clasificacion.        #
##                                                                                      #
#########################################################################################        
                
#========================================================================================
# INICIALIZAR PROGRESS BAR.
#========================================================================================

withProgress(message= "Classifying.", value= 0, {                   
        
#========================================================================================
# CARGADO DE DATOS Y MODELOS A ENTRENADOS.
#========================================================================================        
        
# Primer incremento de la barra de progreso.
incProgress(0.25, detail= "Loading trained model...")
        
        logistic_classification <- TRUE   # sino hace svm_classification
        if(logistic_classification) {
                
                # Cargamos el modelo entrenado.
                #load("./modelos/WS_logist_5000_76point5.RData")
                #load("./modelos/WS_logist_7500_78point5.RData")
                #load("./modelos/WS_logist_19997_81point4.RData")
                load("./modelos/WS_logist_19997_77point5.RData")

        } else {
                # Cargamos el modelo entrenado.
                load("./modelos/WS_svm_5000_67point3.RData")
                #load("./modelos/WS_svm_19997_77point7.RData")
        }

#========================================================================================
# PREPROCESADO DE LOS TEXTOS
#========================================================================================

# Segundo incremento de la barra de progreso.
incProgress(amount= 0.25, detail= "Pocessing text..." )
        
        source("./scripts/process_texts.R")
        txt_corpus <- process_texts(texto, stem= TRUE)                

        # Sacamos el contenido de texto de la estructura corpus a un vector de textos.
        proc_text <- as.character(lapply(txt_corpus, '[[', "content" ))
       
#========================================================================================
# PROYECCION TEXTO SOBRE MATRIZ TFIDF
#========================================================================================

# Tercer incremento de la barra de progreso.
incProgress(amount= 0.25, detail= "Text to TF-IDF vector..." )
        
        # terminos de la matriz tfidf.
        terminos <- colnames(Xtrain)
        
        # termFreq() funcion del paquete "TM".
        tf_text <- termFreq(proc_text) 
        
        # Nos quedamos con aquellos terminos de 'tf_text' coincidentes con los de tfidf. 
        tf_text <- tf_text[names(tf_text) %in% terminos]
        
        # Preparamos el contenedor para generar el vector de terminos del texto. 
        # Debe tener todos los terminos de tfidf.
        text_vec <- numeric(length= length(terminos))
        names(text_vec) <- terminos
        
        # Escribimos en text_vec los valores de frecuencias de tf_text.
        idx <- match(names(tf_text), terminos)
        text_vec[idx] <- tf_text
        
        # Pesamos con idf.
        text_vec_idf <- text_vec * idf
        
        # Aplica normalizacion (en caso que a tfidf se la normalize!!).
        with.normalize <- TRUE
        if(with.normalize) {
                text_vec_idf <- text_vec_idf/sqrt(sum(text_vec_idf^2))
                
                # Documentos vac?os (filas de 0) aparecen NaN al dividir por 0.
                text_vec_idf[is.nan(text_vec_idf)] <- 0  # asignamos 0 a los NaN. 
        }        
        
        Xpred <- t(as.matrix(text_vec_idf))
  
#========================================================================================
# CLASIFICACION DE TEXTO.
#========================================================================================

# Cuarto incremento de la barra de progreso.
incProgress(amount= 0.25, detail= "Text classification..." )
          
        if(logistic_classification) {

                 source("./scripts/predict.logist.R") 
                 pred <- predict.logist(logist_model$weights, Xpred)
                 text_class <- pred$prediction
                 text_prob <- pred$norm_probs
                 colnames(text_prob) <- as.character(1:20)

         } else {
                 library("e1071")
                 pred <- predict(svm_model, Xpred, probability= TRUE)
                 text_class <- as.numeric(pred)
                 text_prob <- attributes(pred)$probabilities
         }
        
        grupos <- sort(unique(DT_sample$Tag))
        
        tratayPredice <- list(grupo= grupos[text_class], 
                              prob= text_prob, 
                              grupos= grupos)

        
}) #end withProgress 
}