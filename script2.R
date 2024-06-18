library(geoR)

funcao_exercicio <- function(n = 100, phi.v = c(0.001,0.1,0.2,0.5),nugget = 3 ){
  for(phi in phi.v){
    resultado <- list()
    pop <- grf(n = 1600,
               grid = "reg",
               cov.pars = c(1,phi),
               cov.model = 'matern',
               nugget = nugget)
    
    amostra <-  pop |>
      as.data.frame()
    amostra <- amostra[sample(nrow(amostra),n),] |> as.geodata()
    #Exercicio A
    #Parametros reais
    gr <- pred_grid(amostra$borders)
    KC <- krige.control(cov.model = 'matern',
                        cov.pars = c(1,phi), 
                        nugget = nugget, 
                        kappa = 0.5)
    OC <- output.control(n.pred = 1000, simul = TRUE)
    
    pred.real <- krige.conv(amostra, loc = amostra$coords, borders = amostra$borders,
                       krige = KC, out = OC)
    #Parametros Estimados
    ml <- likfit(amostra,ini = c(1, 0.05), nug = nugget,cov.model = 'matern')

  }
}
