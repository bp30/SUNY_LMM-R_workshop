## Significant test of whether MLM is needed between null model and base model

basenull_logtest <-function (base_model, null_model){
  x1<-logLik (base_model)
  y1<- logLik (null_model)
  LR_cont <- 2*(x1-y1)
  print (paste('loglik base:', x1))
  print (paste('loglik null:', y1))
  return (paste('p:',pchisq(LR_cont[1], df = 1)))
}
