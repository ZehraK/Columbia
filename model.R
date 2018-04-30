setwd("C:/Users/Columbia/Desktop/yarisma/ilce_data")
ilce_i <- read.csv("ilce_i.csv")

#A few additional cleansing before model
#regional price to reduce factor levels
     emlakendeksi <- aggregate((as.numeric(ilce_i$fiyat)), list(ilce_i$mahalle), mean)
     colnames(emlakendeksi) <- c("mahalleler", "ortalama_fiyat")
     ilce_i$mahalle_ortalama_fiyat <- emlakendeksi$ortalama_fiyat[match(ilce_i$mahalle, emlakendeksi$mahalleler)]

#convert 2 factor variables into numeric
     ilce_i$site_icerisinde <- ifelse(ilce_i$site_icerisinde=="evet", 1, 0)
     ilce_i$banyo_sayisi <- ifelse(ilce_i$banyo_sayisi=="yok", 0, ifelse(ilce_i$banyo_sayisi=="1", 1, ifelse(ilce_i$banyo_sayisi=="2", 2, 3)))

#eliminate the features with multicollinearity issues
      selected_features <- c("fiyat","ic_firin", "ic_giyinme_odasi", "ic_gomme_dolap", "ic_klima", "ic_mutfak_ankastre",
                       "ic_set_ustu_ocak","ic_vestiyer", "ic_camasir_odasi","dis_guvenlik",
                       "dis_otopark", "engelli_giris_rampa", "muhit_market", "muhit_ilkogretim",
                       "cephe_puaný", "ulasim_e5", "ulasim_marmaray", "ulasim_tren_istasyonu",
                       "manzara_bogaz", "manzara_deniz", "manzara_gol", "manzara_doga", "manzara_havuz", 
                       "mahalle", "manzara_sehir", "mahalle_ortalama_fiyat", "site_icerisinde", "banyo_sayisi",
                       "oda_sayisi", "isitma","bulundugu_kat",  "balkon", "kullanim_durumu",
                       "satici_turu", "kat_konumu")    

     ilce_i <- ilce_i[,selected_features]

#####MODEL

  #partition data
     set.seed(12345)
     ind <- sample(2, nrow(ilce_i), replace = T, prob=c(0.8, 0.2))
     train <- ilce_i[ind==1,]
     test <- ilce_i[ind==2,]

  #create matrix- one hot encding for factor variables
     trainm <- sparse.model.matrix(fiyat~.-1, data=train) 
     train_label <- train[,"fiyat"]
     train_matrix <- xgb.DMatrix(data=as.matrix(trainm), label=train_label)

     head(trainm)

     testm <- sparse.model.matrix(fiyat~.-1, data=test)
     test_label <-test[,"fiyat"]
     test_matrix <- xgb.DMatrix(data=as.matrix(testm), label=test_label)

  #parameters
     xgb_params <- list( "booster" = "gblinear", 
                    "objective" = "reg:linear",
                    "eval_metric" = "rmse")
     watchlist <- list(train=train_matrix, test=test_matrix)

   #XGB Model
     set.seed(12345)
     bst_model <- xgb.train (data= train_matrix,
                             xgb_params,
                             nrounds=500,
                             eta=0.25,
                             gamma=0.01,
                             watchlist= watchlist,
                             verbose = 1)


  #model performance (some models have different parameter settings but the the deafult values are given below)
     cv.res <- xgb.cv(data = train_matrix, label = train_label, nfold = 5, gamma=0.1,
                 nrounds =500, eta=0.25, objective = "reg:linear", booster="gblinear")
  #predictions
     p <-  predict(bst_model, newdata=test_matrix)

  #the weigth for the model i
     nrow(ilce_1) 


