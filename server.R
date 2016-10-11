library(shiny)
## Define server logic for slider examples
shinyServer(function(input, output) {
    ## Reactive expression to compose a data frame containing all of the values

    ## Show the values using an HTML table
    output$densidad <- renderPlot({
        require(plot3D)
	mx  = input$mx
	my  = input$my
	sx  = input$sx
	sy  = input$sy
	rho = input$rho
 	bivariada = function(x,y,mx=0,my=0,sx=1,sy=1,rho=0){
            xx  = (x-mx)/sx
            yy  = (y-my)/sy
            K   = 2*pi*sx*sy*sqrt(1-rho^2)
            res = exp(-(xx^2+yy^2-2*rho*xx*yy)/(2*(1-rho^2)))/K
            return(res)
	}
        N    = input$grilla
	a    = 5
	xx   = seq(-a,a,length=N)
	yy   = seq(-a,a,length=N)
	zz   = outer(xx,yy,FUN=bivariada,mx=mx,my=my,sx=sx,sy=sy,rho=rho)
        par(bty="l")
        calor = NULL
        if(input$calor){
            calor = "gray"
        }
	persp3D(x=xx,y=yy,z=zz, colvar=zz, phi = input$phi, theta = input$theta,
                box = T, axes=T, expand =.5,
                col = calor, border="black",
                ticktype="detailed",
                ltheta=75,lphi=45)
    })
    
    output$contorno <- renderPlot({	
	mx  = input$mx
	my  = input$my
	sx  = input$sx
	sy  = input$sy
	rho = input$rho
 	bivariada = function(x,y,mx=0,my=0,sx=1,sy=1,rho=0){
            xx  = (x-mx)/sx
            yy  = (y-my)/sy
            K   = 2*pi*sx*sy*sqrt(1-rho^2)
            res = exp(-(xx^2+yy^2-2*rho*xx*yy)/(2*(1-rho^2)))/K
            return(res)
	}
	N    = input$grilla
	a    = 5
	xx   = seq(-a,a,length=N)
	yy   = seq(-a,a,length=N)
	zz   = outer(xx,yy,FUN=bivariada,mx=mx,my=my,sx=sx,sy=sy,rho=rho)
        contour(xx,yy,zz,xlab="X",ylab="Y",asp=1)
        grid(ncol(zz))
    })

    output$condicional <- renderPlot({	
	mx  = input$mx
	my  = input$my
	sx  = input$sx
	sy  = input$sy
	rho = input$rho
        y0  = input$y0
 	bivariada = function(x,y,mx=0,my=0,sx=1,sy=1,rho=0){
            xx  = (x-mx)/sx
            yy  = (y-my)/sy
            K   = 2*pi*sx*sy*sqrt(1-rho^2)
            res = exp(-(xx^2+yy^2-2*rho*xx*yy)/(2*(1-rho^2)))/K
            return(res)
	}
	N    = input$grilla
	a    = 5
	xx   = seq(-a,a,length=N)
	zz   = bivariada(xx,y0,mx=mx,my=my,sx=sx,sy=sy,rho=rho)
        plot(xx,zz,xlab="X",ylab="f",ylim=c(0,0.2),type="l",lty=1,lwd=2,col="blue")
        grid(ncol(zz))
    })

    
    ##barplot(colSums(input$sliderValues[,-1])))
    ##    output$values <- renderTable({
    ##      sliderValues()
})
