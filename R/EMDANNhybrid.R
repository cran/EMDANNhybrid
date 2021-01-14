#EMD based ANN model
EMDANNhybrid=function(data,k,l,n,r,m)
  {
  data_org=as.matrix(data)
  xt=as.matrix(data_org)
  xt=as.vector(data_org)
  xt=ts(xt)
  #EMD
  #code for display no.of imf and residual
  try=emd(xt,boundary = "wave")
  imf_extr=try$imf
  emd_residual=try$residue
  no_of_imf=try$nimf
  len_extr_imf=length(imf_extr[,1])
  length_split=len_extr_imf-1
  test_data_l=ceiling(k*length_split)
  train_data_original=data_org[1:test_data_l,]
  len_ogdata=length(xt)
  test_data_original=data_org[(test_data_l+1):len_extr_imf,]
  length_test_data=length(test_data_original)
  # dataset creation
  extr_imf=0
  model_Ann=0
  predicted_out=matrix(nrow =length_test_data,ncol = no_of_imf)
  MSE_out=0
  RMSE_out=0
  MAPE_out=0
  MAD_out=0
  final_predict_imf=0
  for (i in 1:no_of_imf)
  {
    data=imf_extr[,i]
    data=as.vector(data)
    data=as.matrix(data)
    len_data=length(data)
    split_train=k*len_data
    r_train=ceiling(split_train)
    traindata=data[1:r_train,]
    testdata=data[(r_train+1):len_data,]
    model_Ann<-nnetar(traindata, Lag= l,size=n, repeats=r, maxit=m)
    t=length(testdata)
    forecasted<- forecast(model_Ann,h=t)
    forecasted_value=forecasted$mean
    predicted_out[,i]=as.vector(forecasted_value)
    final_predict_imf=final_predict_imf+predicted_out[,i]
  }
   lenght_of_residual=length(emd_residual)
  #differencing
  dif_resid=diff(emd_residual)
  len_dresid=length(dif_resid)
  #spliting of data set

  datar=ts(dif_resid)
  datar=as.vector(datar)
  datar=as.matrix(datar)
  len_datar=length(datar)
  split_trainr=k*len_datar
  r_trainr=round((split_trainr))
  traindatar=datar[1:r_trainr,]
  testdatar=datar[(r_trainr+1):len_datar,]

  model_Annr<-nnetar(traindata, Lag= l,size=n, repeats=r, maxit=m)

  #out sample
  t_r=length(testdatar)
  forecastedr<- forecast(model_Annr,h=t_r)
  forecasted_valuer=forecastedr$mean
  predicted_outr=as.vector(forecasted_valuer)
  final_prediction=final_predict_imf+predicted_outr
  # summarize accuracy
  MSE_out <- mean((test_data_original - final_prediction)^2)
  RMSE_out<- sqrt(MSE_out)


  #mean absolute deviation (MAD)
  MAD_out<-mean(abs(test_data_original - final_prediction))


  #Mean absolute percent error (MAPE)
  MAPE_out=mean(abs((test_data_original-final_prediction)/test_data_original))
  prediction_accuracy=cbind(RMSE_out,MAD_out,MAPE_out)
  output_f=list(final_prediction,prediction_accuracy)
  return(output_f)
}


