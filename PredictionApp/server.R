shinyServer(
  function(input, output){
    library(BatchGetSymbols)
    library(jmuOutlier)
    most_advanced_symbol 
    
    valid = list()
    
    count = 1
    
    for(i in 1:5) {
      first.date <- Sys.Date()-250 
      last.date <- Sys.Date()
      tickers <- most_advanced_symbol[i] 
      
      l.out <- BatchGetSymbols(tickers = tickers,
                               first.date = first.date,
                               last.date = last.date)
      stock.data <- l.out$df.tickers
      
      if(length(stock.data$price.close) != 0) {
        
        valid[count] <- most_advanced_symbol[i]
        
        count = count + 1
        
      }
      
    }    
    
    first.date <- Sys.Date()-250 
    last.date <- Sys.Date()
    
    tickers <- valid[[1]]
    
    l.out <- BatchGetSymbols(tickers = tickers,
                             first.date = first.date,
                             last.date = last.date)
    stock.data <- l.out$df.tickers 
    
    close <- stock.data$price.close
    
    s1 <- close[2:length(close)]
    s0 <- close[1:length(close)-1]
    bs.val <- log(s1/s0)
    hist(bs.val, freq = F, breaks = 40)
    sd = sd(bs.val)
    p.norm.val <- p.cauch.val <- p.laplace.val <- numeric(length(bs.val))
    for(i in 1:length(bs.val)){
      p.norm.val[i] <- ks.test(bs.val[-i], 'pnorm', sd=sd)$statistic
      p.cauch.val[i] <- ks.test(bs.val[-i], 'pcauchy', scale=sd)$statistic
      p.laplace.val[i] <- ks.test(bs.val[-i], 'plaplace',sd=sd)$statistic
    }
    mod.vals <- c(mean(p.norm.val), mean(p.cauch.val), mean(p.laplace.val))
    
    val.min <- which(mod.vals == min(mod.vals))
    if(val.min == 1){best.mod = 'normal'
    }else if(val.min == 2){best.mod = 'cauchy'
    }else if(val.min == 3){best.mod = 'laplace'
    }else{stop('Incorrect value for val.min')}
    best.mod
    
    rnorm1=function(n,a,b){
      set.seed(123)
      normal_sample=integer(0)
      for (i in 1:n){
        u=runif(1)
        v=runif(1)
        z=sqrt(-2*log(u))*cos(2*pi*v)
        normal_sample=c(normal_sample,z)
      }
      return (a+b*normal_sample)
    }
    
    
    
    
    output$lineplot <- renderPlot({
      if (input$Stocks == "AMWD Trend Line"){
        plot(x = stock.data$ref.date, y = close, type = "l", xlab = "Trading Date", ylab = "Stock Price in US Dollars ($)",
             col = "red", fg = "blue", col.lab = "blue")
        
        if (input$smoothline){
          trendcurve <- lowess(x = as.numeric(stock.data$ref.date), y = close, f = input$s)
          lines(trendcurve, col = "blue", lwd = 1)
          
        }
      }
      if (input$Stocks == "AMWD (American Woodmark Corporation)"){
        if (input$Method == "MCMC Prediction"){
          set.seed(0)
          
          rw.normal=function(sigma,x0,N){
            x=numeric(N)
            x[1]=close[[1]]
            u=runif(N)
            k=0
            for (i in 2:N){
              y=rcauchy(1,x[i-1],sigma)
              ratio=dnorm(y,mean(close),1)/dnorm(x[i-1],mean(close),1)
              accept=u[i]<=ratio
              x[i]=y*accept+x[i-1]*(1-accept)
              k=k+accept
            }
            return(list(x=x,k=k))
          }
          N=2000+length(bs.val)
          sigma=sd
          rw1=rw.normal(sigma,x0,N)
          b0=2000
          index=(b0):N
          y1=rw1$x[index]
          
          plot(x = stock.data$ref.date, y = close, type = "l", xlab = "Trading Date", ylab = "Stock Price in US Dollars ($)",
               col = "red", fg = "blue", col.lab = "blue")
          par(new=TRUE)
          
          plot(x = stock.data$ref.date, y = y1, type = "l", col = "blue", xlab = "Trading Date", ylab = "Stock Price in US Dollars ($)", fg = "blue", col.lab = "blue")
        }
        
        
        if (input$Method == "Bootstrap Prediction"){
          
          set.seed(0)
          n = length(bs.val)
          B = length(stock.data$ref.date) 
          Tboot = numeric(B)
          
          price_today = close[length(close)]
          
          for(i in 1:B) {
            
            bootstrap_price_var = sample(bs.val, n, replace= TRUE)
            
            Tboot[i] = mean(bootstrap_price_var)
            
          }
          
          estimated_price_var = mean(Tboot)
          
          stock_prediction = function(s0, t, v, dist) {
            
            if(dist == "normal") {
              
              for(i in 1:t) {
                
                s0 = s0 * exp(rnorm1(1, 0, sqrt(v)))
                
              }
              
              return(s0)
              
            }
            
            else if(dist == "cauchy") {
              
              for(i in 1:t) {
                
                s0 = s0 * exp(rcauchy1(1, 0, sqrt(v)))
                
              }
              
              return(s0)
              
            }
            
            else if(dist == "laplace") {
              
              for(i in 1:t) {
                
                s0 = s0 * exp(rlaplace1(1, 0, sqrt(v)))
                
              }
              
              return(s0)
              
            }
            
          }
          
          result = numeric(B)
          
          for(i in 1:B) {
            
            result[i] = stock_prediction(close[1], 20, estimated_price_var, best.mod)
            
          }
          
          plot(x = stock.data$ref.date, y = close, type = "l", xlab = "Trading Date", ylab = "Stock Price in US Dollars ($)",
               col = "red", fg = "blue", col.lab = "blue")
          
          par(new=TRUE)
          
          plot(x = stock.data$ref.date, y = result, type = "l", col = "blue", xlab = "Trading Date", ylab = "Stock Price in US Dollars ($)", fg = "blue", col.lab = "blue")
          
          
          
          
          
          
        }    
      } 
      
      if (input$Stocks == "AMWD Return Estimation"){
        rw.normal=function(sigma,x0,N){
          x=numeric(N)
          x[1]=close[[1]]
          u=runif(N)
          k=0
          for (i in 2:N){
            y=rcauchy(1,x[i-1],sigma)
            ratio=dnorm(y,mean(close),1)/dnorm(x[i-1],mean(close),1)
            accept=u[i]<=ratio
            x[i]=y*accept+x[i-1]*(1-accept)
            k=k+accept
          }
          return(list(x=x,k=k))
        }
        N=2000+length(bs.val)
        sigma=sd
        rw1=rw.normal(sigma,x0,N)
        b0=2000
        index=(b0):N
        y1=rw1$x[index]
        plot(y1,type="l",ylab="predicted price",xlab="day")
        y1[length(close)-1] # Last predicted price
        hundreddaysprediction <- rw.normal(sigma, y1[length(close)-1], 100)$x + 38
        
        newtimeline <- seq(Sys.Date(), Sys.Date() + 99, by = "day")
        
        plot(x = newtimeline, y = hundreddaysprediction, type = "l", lty = 2, col = "red", xlab = "Future Trading Days", ylab = "Stock Price in US Dollars ($)", fg = "blue", col.lab = "blue")
        
        
        
        
        
      }       
      
    }, height = 600, width = 1000)
    
    
    
    
    
    
  }       
)





