\name{EMDANNhybrid}
\alias{EMDANNhybrid}
 \title{New Ensemble Hybrid Machine Learning Model}
\usage{
EMDANNhybrid(data,k,l,n,r,m)
}
\description{The researchers can use this package to fit Empirical Mode Decomposition and Artificial Neural Network based hybrid model for nonlinear and non stationary time series data. It will also provide you with accuracy measures along with an option to select the proportion of training and testing data sets.User can get to choose appropiate lag with tuning parameter like maximum iterations for training the neural model}

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
\details{the package implemets an ensemble hybrid approach for forecasting nonlinear and nonstationary time series data proposed by Choudhary et al (2019). In this method, EMD is to disintegrate a non-stationary and nonlinear time series data into several simple modes (IMFs and residue). Each of these modes further forecasted using artificial neural network model. finally the all forecasted values are aggregated for final forecast value.
}


\value{It returns the accuracy measures of the fitted model.
}

\references{
  %%
    Choudhary et al.(2019)<http://www.isee.org.in/uploadpaper/55,January%20-%20March,13.pdf>
}
\author{
  Pankaj Das,Achal Lama,Girish Jha
}
\seealso{EMD,nnfor}
\examples{
  set.seed(6)
  data=rnorm(300,6.6,.36)
EMDANNhybrid(data,0.7,1,5,20,120)
}
\keyword{Hybrid}
\keyword{Machine Learning}
\keyword{EMD}
\keyword{ANN}
