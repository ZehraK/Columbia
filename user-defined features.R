  #mkarefunc

   mkarefunc <- function(data, a){
          b <- ifelse((a>=1000 & a<10000), a/10, ifelse((a>=10000 & a<100000),a/100, ifelse((a>=100000 & a<1000000), a/1000, ifelse((a>=1000000), a/10000, a)) ) )
          b <- as.numeric(b)
         return(b)
   }

 #cephe_puanı
   cephefunc <- function(kuzey, guney, dogu, bati) {
          cephe_puani <- kuzey*(2.325e-02) + guney*(-2.560e-02) + bati*(5.979e-02) + dogu*7.727e-05
          return(cephe_puani)
   } 

 #Function for kat konumu 1- zemin 2-kot 3-kat 4-diğer(müstakil/villa)

   katfunc <- function(data, a){
          b <- ifelse(a == "giris_kati" | a== "bahce_kati" | a =="zemin_kat"| a == "yuksek_giris", "zemin", ifelse(a == "mustakil" | a == "villa_tipi" , "diger", ifelse(a== "kot_1" | a == "kot_2"| a == "kot_3" | a == "kot_4", "kot", "kat"))                  )
          b <- as.factor(b)
          return(b)
    }


