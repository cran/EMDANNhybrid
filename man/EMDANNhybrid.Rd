\name{EMDANNhybrid}
\alias{EMDANNhybrid}
 \title{New Ensemble Hybrid Machine Learning Model}
\usage{
EMDANNhybrid(data,k,l,n,r,m)
}
\description{The researchers can use this package to fit empirical mode decomposition and artificial neural network (also known as TDNN) based hybrid model for nonlinear and non stationary time series data. It will also provide you with accuracy measures along with an option to select the proportion of training and testing data sets. User can get to choose appropiate lag with tuning parameter like maximum iterations for training the neural model.}

%-
  \arguments{

    \item{data}{

       A univariate time series data
    }
    \item{k}{

      Partition value for spliting the data set into training and testing sets

    }
    \item{l}{
      The lag length for fitting neural network model
    }
    \item{n}{
      Size of the hidden node for fitting neural network model
    }
    \item{r}{
      Number of networks to fit with different random starting weights
    }
    \item{m}{
      Maximum number of iterations for fitting neural network model
    }
  }
\details{The package implemets an ensemble hybrid approach for forecasting nonlinear and nonstationary time series data proposed by Choudhary et al. (2019), Das (2019) and Das et al. (2020,2022,2023). In this method, EMD is to disintegrate a non-stationary and nonlinear time series data into several simple modes (IMFs and residue). Each of these modes further forecasted using artificial neural network model. finally the all forecasted values are aggregated for final forecast value.
}


\value{

\item{Prediction_Accuracy_EMDANN }{List of performance measures of the fitted EMDANN model.}

  \item{Final_Prediction_EMDANN }{Final forecasted value of the VMD based ANN model. It is obtained by combining the forecasted value of all individual IMF and fresidue.}
}

\references{
  Das,P., Jha, G. K., Lama, A., Parsad, R. and Mishra, D. (2020). Empirical Mode Decomposition based Support Vector Regression for Agricultural Price Forecasting. Indian Journal of Extension Education, 56(2): 7-12. (https://www.indianjournals.com/ijor.aspx?target=ijor:ijee3&volume=56&issue=2&article=002).

Das, P., Jha, G. K. and Lama, A. (2023). Empirical Mode Decomposition Based Ensemble Hybrid Machine Learning Models for Agricultural Commodity Price Forecasting. Statistics and Applications. 21(1),99-112.(https://ssca.org.in/media/9_SA31042022_R1_SA_17042022_FINAL_Finally_Pankaj_Das_Empirical_mode.pdf).

Das, P., Jha, G. K., Lama, A. and Bharti (2022). EMD-SVR Hybrid Machine Learning Model and its Application in Agricultural Price Forecasting. Bhartiya Krishi Anusandhan Patrika. (DOI: 10.18805/BKAP385)

Das, P. (2019). Study On Machine Learning Techniques Based Hybrid Model for Forecasting in Agriculture. Unpublished Ph.D. Thesis.

Choudhury, K., Jha, G. K., Das, P. and Chaturvedi, K. K. (2019). Forecasting Potato Price using Ensemble Artificial Neural Networks. Indian Journal of Extension Education, 55(1):71-77.
(https://www.indianjournals.com/ijor.aspx?target=ijor:ijee3&volume=55&issue=1&article=013).
}

\author{
  Pankaj Das, Achal Lama and Girish Kumar Jha
}
\seealso{EMD,nnfor,EMDSVRhybrid}

\examples{
  set.seed(6)
  data = rnorm(300,6.6,.36)
EMDANNhybrid(data,0.7,1,5,20,120)
}
\keyword{Hybrid}
\keyword{Machine Learning}
\keyword{EMD}
\keyword{ANN}
