#Data cleansing
    
      veri_df <- veri_df[!is.na(veri_df$fiyat),] 
      #use log_fiyat only for data cleansing to clean the extreme values since 
      veri_df$log_fiyat <- log(veri_df$fiyat) 
      veri_df <- veri_df[veri_df$log_fiyat<1,]
      
      veri_df <- veri_df[!is.na(veri_df$m_kare),]
      
      veri_df <- veri_df[(veri_df$banyo_sayisi == "1" | veri_df$banyo_sayisi == "2" |veri_df$banyo_sayisi == "3" | veri_df$banyo_sayisi == "yok"),]
      levels(droplevels(veri_df$banyo_sayisi))
      veri_df$banyo_sayisi <- as.factor(as.character(veri_df$banyo_sayisi))

       
#imputation 1
      
      veri_i
      
      #imputation esyali 
      
      set.seed(3081996)
      veri_i$bina_yasi <- as.numeric(veri_i$bina_yasi)
      veri_i$esyali <- as.factor(veri_i$esyali)
      varname1 <- c('bina_yasi','ic_buzdolabi','muhit_sehir_merkezi')
      r1 <- stats::complete.cases(veri_i[, varname1])
      x1 <- veri_i [r1, varname1]
      y1 <- veri_i[r1, 'esyali']
      ry1 <- !is.na(y1)
      table(ry1)
      #percentage of missing data 
      sum(!ry1)/length(ry1)
      #impute missing krediye_uygun data
      yimpute1 <- mice.impute.logreg(y1, ry1, x1)
      length(yimpute1)
      histogram(yimpute1, xlab = "Eþyalý için Ýmputasyon Deðerleri",
                ylab="Eþyalý için Ýmputasyon Oranlarý(%)",
                col=c("gray"), main = "Eþyalý Ýmputasyon Daðýlýmý")
      #fill in the missings
      veri_i$esyali[is.na(veri_i$esyali)] <- yimpute1 

      #imputation krediye_uygun
      
      set.seed(30081996)
      varname2 <- c('bina_yasi', 'ilce_ortalama_fiyat') #ortalama fiyat ile ilceye iliþkin bilgi de içermiþ oldu
      r2 <- stats::complete.cases(veri_i[, veri_df$varname2])
      x2 <- veri_i [r2, veri_i$varname2]
      y2 <- veri_i[r2, 'krediye_uygun']
      ry2 <- !is.na(y2)
      table(ry2)
      #percentage of missing data 
      sum(!ry2)/length(ry2)
      #imp öncesi grafik????
      #impute missing krediye_uygun data
      yimpute2 <- mice.impute.logreg(y2, ry2, x2)
      length(yimpute2)
      hist(yimpute2, xlab = 'Krediye Uygun Ýmputasyon Deðerleri',ylab="Ev Sayýsý",col="gray48",main="Krediye Uygun Ýmputasyonu")
      #assign the imputation values 
      veri_i$krediye_uygun[is.na(veri_i$krediye_uygun)] <- yimpute2
      
      #handle site_icerisinde
      set.seed(30081996)
      bina_yasi_num <-as.numeric(bina_yasi)
      varname3 <- c('mkare', 'fiyat', 'bina_yasi_num', 'kat_sayisi')
      r3 <- stats::complete.cases(veri_i[, veri_i$varname3])
      x3 <- veri_i [r3, veri_i$varname3]
      y3 <- veri_i[r3, 'site_icerisinde']
      ry3 <- !is.na(y3)
      table(ry3)
      
      #percentage of missing data 
      sum(!ry3)/length(ry3)
      #impute missing site_icerisinde data
      yimpute3 <- mice.impute.logreg(y3, ry3, x3)
      length(yimpute3)
      hist(yimpute3, xlab = 'Imputed missing site icerisinde')
      #assign the imputation values to the nas in df
      veri_i$site_icerisinde[is.na(veri_i$site_icerisinde)] <- yimpute3
      
      
      #imputation balkon NMAR, factor olduðu için na'leri yeni bir seviye kabul edebiliriz.
      levels <- levels(veri_i$balkon)
      levels[length(levels) + 1] <- "bilinmiyor"
      veri_i$balkon <- factor(veri_i$balkon, levels = levels)
      veri_i$balkon[is.na(veri_i$balkon)] <- "bilinmiyor"
      
      
      
      #sparse matrix to impute aidat
      veri_i_matrix <- data.matrix(veri_i)
      
      #imputation aidat MAR
      set.seed(30081996)
      varname_aidat <- c("fiyat","ilce_ortalama_fiyat","ilce_num","bina_yasi","site_icerisinde")
      r <- stats::complete.cases(veri_i_matrix[,veri_i$varname_aidat])
      x <- veri_i_matrix[r, veri_i$varname_aidat]
      y <- veri_i_matrix[r, 'aidat']
      ry <- !is.na(y)
      table(ry)
      #percentage of missing data
      sum(!ry)/length(ry)
      #impute missing aidat 
      yimpute4 <- mice.impute.pmm(y,ry,x)
      length(yimpute4)
      hist(yimpute4, xlab= 'Aidat Ýçin Ýmputasyon Deðerleri',ylab="Ev Sayýsý", main="Aidat Ýmputasyonu",col="mistyrose4")
      veri_i$aidat[is.na(veri_i$aidat)] <- yimpute4
      
      
#imputation 2
      veri_ii <- veri_ii[!is.na(veri_ii$esyali),]
      veri_ii <- veri_ii[!is.na(veri_ii$site_icerisinde),]
      veri_ii <- veri_ii[!is.na(veri_ii$krediye_uygun),]
      veri_ii <- subset(veri_ii, select=-c(1,12,16))




