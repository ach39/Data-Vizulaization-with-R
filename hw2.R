#
# @author : achauhan39 (GTid:903271003)
# @date   : Feb-07-2017
# @description : File code for Q1,2,4 and 5 for HW-2.
# 
# @usage : code for these questions in encapsulated in  functions.
#         To test code for Q1a call Q1a().  
#         To test code for Q1b call Q1b().
#         Similary code for Q2 and Q5 can be tested via Q2() and Q5()
#         To test  Q4 call
#             compare_filesize(lo=0, hi=10000, sample_cnt = 10)
#
#


library(ggplot2)
library(dplyr)
library(moments)
library(memisc)
library(RColorBrewer)
library(GGally)
library(gridExtra)

data(midwest)
data(diamonds)
attach(midwest)
attach(diamonds)
DBG <- 0



#*******************************************************************************
# Q1.a Describe the relationship between the states in the Midwest region
# and the percentage of people that have a professional employment
# 
#
#*******************************************************************************
Q1a <- function() {
  data(midwest)
  attach(midwest)
  
  # get summary of %professional by state
  by(percprof,state,summary)
  
  # Boxplot for %professional(percprof) by state
  ggplot(midwest , aes(reorder(state, -percprof, median) , percprof)) + 
    geom_boxplot(aes(color=state)) +
    coord_flip()+
    scale_x_discrete("state") +
    ggtitle("% of professionals by state")
  
  #find which county has min and max percprof
  subset(midwest,percprof==min(percprof))[, c('state','county','percadultpoverty','percollege')]
  subset(midwest,percprof==max(percprof))[, c('state','county','percadultpoverty','percollege')]
  
  #summarize stats %professional by state for easy comparison
  tbl_procprof =  midwest  %>%
    group_by(state) %>%
    summarise(mean = mean(percprof) , 
              median = median(percprof),
              min =min(percprof),
              max =max(percprof),
              IQR = IQR(percprof),
              count=n())
  print.data.frame(tbl_procprof, digits=3)
}

#*******************************************************************************
# Q1.b Describe which state has the lowest and which state has the highest
#  percentage of total population with a professional employment
#
# 1.	Compute raw count of professional   : percprof  *  popadult
# 2.	Compute sum of raw professional# and adult population by state.
# 3.	Compute %professionalsby state :  sum of raw prof#/sum of adult pop
# 4.  Plot the data
#
#
#*******************************************************************************
Q1b <- function() {
  # step 1 : Compute raw count of professional
  midwest$prof_cnt <- percprof * popadults  
  
  # step 2,3 : Compute sum of raw professional# and total population by state
  tbl =  midwest  %>%
    group_by(state) %>%
    summarise(sum_adult =sum(popadults) , 
              sum_prof = sum(prof_cnt),
              perc_prof = sum(prof_cnt)/sum(popadults))
  print(tbl)
  
  # step 4 : Plot the data
  ggplot(tbl, aes(x=reorder(tbl$state,tbl$perc_prof), y =tbl$perc_prof)) + 
    geom_bar(stat='identity', aes(fill=state)) +
    ylab("% professionals") +
    scale_x_discrete("state") +
    geom_text(aes(label= round(tbl$perc_prof,2)), vjust=2) +
    ggtitle("% professionals by state ")
  
}
  

#*******************************************************************************
# Q2. 
#
#
#
#*******************************************************************************

ggplot(data=midwest,aes(x=perchsd,y=percollege)) + geom_point(aes(color=state)) + geom_smooth(se=FALSE)

ggplot(data=subset(midwest,perchsd>75) ,aes(x=perchsd,y=percollege,color=state)) + 
  ggtitle("%hsd vs. %college by state where %hsd<75")+
  geom_point()+ geom_smooth(se=FALSE)


ggplot(data=midwest,aes(x=perchsd,y=percollege,color=state)) + geom_smooth(se=FALSE) + facet_wrap(~inmetro)

ggplot(data=midwest,aes(x=perchsd,y=percchildbelowpovert,color=state)) + geom_smooth(se=FALSE) + facet_wrap(~inmetro)

ggplot(data=midwest,aes(x=perchsd,y=percchildbelowpovert,color=state)) +geom_point() + facet_wrap(~inmetro)


Q2 <- function() {
  # compute raw hsd and college count
  midwest$hsd_cnt = perchsd * popadults
  midwest$college_cnt = percollege * popadults
  
  # compute % hsd and %college over adult population
  midwest$perchsd_adultpop = (perchsd * popadults)/popadults
  midwest$percollege_adultpop = (percollege * popadults)/popadults
  attach(midwest)
  
  # plot %hsd vs %college (over adult pop)
  qplot(perchsd, percollege, data=midwest , color=state, 
        main= "%hsd vs %college over Adult poplation")
  cor.test(perchsd, percollege)
  
  
  # plot %hsd vs %college for WI and IL only
  qplot(perchsd_adultpop, percollege_adultpop, data=subset(midwest, (state=='WI' | state=='IL')) , 
        color=state, main= "%hsd vs %college over Total poplation") + facet_wrap(~state)
  
  # plot %hsd vs %college and facet_wrap by state
  qplot(perchsd_adultpop, percollege_adultpop, data=midwest , 
        main="% hsd vs % college by state") + 
    geom_smooth()+
    xlab(" % high school diploma") +
    ylab(" % College graduates") +
    geom_vline(xintercept = 75, linetype = "dotdash", color='purple') +
    geom_hline(yintercept = 20, linetype = "dotdash", color='purple') +
    facet_wrap(~state)
  
  #summarize stats by state
  tbl_2 = midwest %>%
    group_by(state)%>%
    summarize(sum_pop =sum(popadults),
              sum_hsd = sum(hsd_cnt),
              sum_college = sum(college_cnt),
              perc_hsd = sum(hsd_cnt)/sum(popadults),
              perc_college = sum(college_cnt)/sum(popadults),
              
              mean_hsd = mean(perchsd),
              median_hsd = median(perchsd),
              IQR_hsd = IQR(perchsd),
              sd_hsd = sd(perchsd),
              
              mean_college = mean(percollege),
              median_college = median(percollege),
              IQR_college = IQR(percollege),  
              cor = cor.test(perchsd,percollege)$estimate,
              sd_college = sd(percollege),
              count =n())
  
  print.data.frame(tbl_2,digits=3 )
                   
  # Plot aggregate %hsd and %college by state
  ggplot(tbl_2, aes(x=perc_hsd, y=perc_college)) + geom_point(aes(shape=state)) + 
    geom_label(aes(label=tbl_2$state), hjust=-.2) +
    ggtitle("%hsd vs. %College by state") +
    xlab(" % of High School diploma") +
    ylab("% of College graduates")
    
  l = c('state','perc_hsd','perc_college', 'median_hsd' , 'median_college', 'mean_hsd' , 'mean_college',
        'IQR_hsd', 'IQR_college' ,'cor')
  print.data.frame(tbl_2[,l], digits=3 )
  
  
  # boxplot - perchsd by state
  ggplot(midwest , aes(reorder(state, -perchsd, median) , perchsd)) + 
    geom_boxplot(aes(color=state)) +
    coord_flip()+
    scale_x_discrete("state") +
    ggtitle("% High School Diploma by state")
  
  # boxplot - percollege by state
  ggplot(midwest , aes(reorder(state, -percollege, median) , percollege)) + 
    geom_boxplot(aes(color=state)) +
    coord_flip()+
    scale_x_discrete("state") +
    ggtitle("% College graduates by state")
  
  by(perchsd,state,summary)
  by(percollege,state,summary)
  
  
  ## %hsd vs. college by metro 
  qplot(perchsd, percollege, data=midwest, main= "%hsd vs %college by metro") +
    geom_hline(yintercept = 20,linetype = "longdash", color='blue') + 
    facet_wrap(~inmetro)
  
  ## %below poverty vs. % hsd 
  qplot(percchildbelowpovert,perchsd, data=midwest, main= "%hsd vs % Child below poverty") +
    xlab("% Child below poverty") + ylab("% hsd") + geom_smooth(method=lm) + 
    geom_vline(xintercept = 20, linetype = "longdash", color='blue') +
    facet_wrap(~state)
  
  cor.test(perchsd,percbelowpoverty)
  cor.test(perchsd,percchildbelowpovert)
  
  qplot(percbelowpoverty,perchsd, data=midwest, main= "%hsd vs % below poverty") +
    xlab("% below poverty") + ylab("% hsd") +
    geom_vline(xintercept = 15, linetype = "longdash", color='blue') +
    geom_hline(yintercept = 80, linetype = "longdash", color='blue') +
    facet_wrap(~inmetro)
  
  ## %below poverty vs. %college
  qplot( percbelowpoverty,percollege, data=midwest, main= "%college vs % below poverty") +
    xlab("% below poverty") +  ylab("% College")+
    facet_wrap(~inmetro)
  
  
  # print ("corrleation : ")
  # for (x in c('IL','IN','MI','OH','WI')) {
  #   tmp=subset(midwest, state==x)
  #   cor = cor.test(tmp$perchsd, tmp$percollege)
  #   cat("\n" , x ,": " , round(cor$estimate,3)  )
  # }

}

#*******************************************************************************
# Q3.  Boxplot vs. Histogram vs. QQplot
#
#*******************************************************************************
Q3 <- function() {
  tmp =subset(midwest , (state == 'MI' | state == 'IL'))
  ggplot(tmp, aes(tmp$state,tmp$popamerindian)) + 
    geom_boxplot(aes(color=state)) +
    ggtitle("Amerindian Population - zoomed view") +
    coord_cartesian(ylim=c(0,1500))
  
  by(tmp$popamerindian,tmp$state,summary)
  
  qplot(popamerindian, data=subset(tmp,popamerindian<=5000) , 
        binwidth=200 , color=I('blue'), main = "Histogram : pop_amerindian - IL , MI" )+ 
    facet_wrap(~state) 
  
  ggplot(subset(tmp, state=='MI') ,aes(x=popamerindian, y=..density..)) + 
    geom_histogram(alpha=0.3) + geom_density(color='blue', size=1) + ggtitle("pop amerindian PDF - MI")
  
  ggplot(subset(tmp, state=='IL') ,aes(x=popamerindian, y=..density..)) + 
    geom_histogram(alpha=0.3) + geom_density(color='red', size=1) + ggtitle("pop amerindian PDF - IL")
  
  ## histogram vs boxplot - Faithful
  ggplot(faithful, aes("",eruptions)) + geom_boxplot() + 
    ggtitle("Boxplot : Faithful-eruption duration")
  
  ggplot(faithful , aes(x = eruptions, y = ..density..)) +
    geom_histogram(alpha = .4) + geom_density(size = 1, color='blue') + 
    ggtitle(" Histogram : Faithful-eruption duration ")
  
  #qqplot
  qqnorm(diamonds$depth ,main="Normal QQ plot for Diamonds - Depth")
  qqline(diamonds$depth)
  qqnorm(diamonds$price, main="Normal QQ plot for Diamonds - Price")
  qqline(diamonds$price)
  ggplot(diamonds,aes(x=price)) + geom_density() + ggtitle("PDF - Price")
  ggplot(diamonds,aes(x=depth)) + geom_density() + ggtitle("PDF - depth")
  
  #ggplot(diamonds,aes(sample=price)) + stat_qq(distribution = qt, dparams = list(10))
  #Comapare QQplot for normal vs, gamma distribution
  x=rnorm(300)
  qqnorm(x, main="QQplot for dataset with normal distribution");qqline(x)
  y=rgamma(300,1)
  qqnorm(y, main="QQplot for dataset with gamma distribution");qqline(y)
  
  # sample dependency -boxplot 
  d1 = rnorm(50,0,1)
  d2 = rnorm(50,0,5)
  
  outliers=c(-7,-8.9,7.5,7.3,10)
  d1 = c(d1,outliers)
  d2 = c(d2,outliers)
  
  x=data.frame(d1=d1,d2=d2)
  p1=ggplot(x, aes("",d1)) + geom_boxplot() + ylim(-10,10) + ggtitle("Normal Dist with Stddev =1")
  p2=ggplot(x, aes("",d2)) + geom_boxplot() + ylim(-10,10) +ggtitle("Normal Dist with Stddev =5")
  grid.arrange(p1,p2,ncol=2)
  
  
}

#*******************************************************************************
# Q4.Compare file size of random scatter plot with increasing N. 
# File formats compared : jpeg,png,ps and pdf
#
#
# Function    : compare_filesize (Q.4)
# Description : compares file_size of scatterplot saved as jpeg,
#               ps,png and pdf for varying N.
#
# Args        : lo,hi - low and high range from which n is to be drawn
#               sample_cnt - number of samples from (lo,hi) range
#
# Returns     : genrates dataframe and plot that shows file-size 
#               as a function of Ncomparison-plot for different formats
#               - jpeg,png,ps and pdf
#
#*******************************************************************************

compare_filesize<-function(lo=50, hi=2000,sample_cnt = 10){
  
  n_vals <- round(seq(lo,hi,length.out=sample_cnt) ,0)
  #n_vals <-c(1,10,100,1000,10000,100000,1000000)
  #sample_cnt = 7
  #lo=1
  #hi=1000000
  
  print(n_vals)
  size_jpeg <- rep(NA,sample_cnt)
  size_png <- rep(NA,sample_cnt)
  size_ps <- rep(NA,sample_cnt)
  size_pdf <- rep(NA,sample_cnt)
  i=1
  
  for(n in n_vals){
    x <- get_filesize(lo,hi,n)
    size_jpeg[i] <- x[1]
    size_png[i] <- x[2]
    size_ps[i] <- x[3]
    size_pdf[i] <- x[4]
    i <- i+1
  }
  
  df = data.frame(N =n_vals,jpeg = size_jpeg, png = size_png, ps =size_ps , pdf = size_pdf )
  print(df)
  ggplot(data=df,aes(x=N)) + 
    geom_line(aes(y=jpeg ,colour='jpeg')) +
    geom_line(aes(y=png,colour='png')) +
    geom_line(aes(y=ps ,colour='ps')) +
    geom_line(aes(y=pdf ,colour='pdf')) +
    scale_colour_manual("", 
                        breaks = c("jpeg", "png", "ps","pdf"),
                        values = c("red", "green", "blue","black")) +
    
    ylab("File size (KB)") +
    ggtitle("File Size comparision") 
    ggsave("hw2_filesize_plot.png")
  
}

# *************************************************************
# Function    : get_filesize (helper function for Q4)
#
# Description : computes file_size of scatterplot (with N points) 
#               saved as jpeg,png,ps and pdf.
#
# Args        : lo,hi - low and high range from which n is to be drawn
#               N - number of points in scatterplot.
#
# Returns     : returns file size of (jpeg,png,ps and pdf) saved plots.
# *************************************************************
get_filesize <- function(lo=50, hi=2000, N = 10){
  x <- runif(N,lo,hi)
  y <- runif(N,lo,hi)
  qplot(x,y, main="scatterplot")
  
  ggsave("test_plot.jpeg")
  ggsave("test_plot.png")
  ggsave("test_plot.ps")
  ggsave("test_plot.pdf")
  
  
  filesize <- c(
    file.info("test_plot.jpeg")$size,
    file.info("test_plot.png")$size,
    file.info("test_plot.ps")$size,
    file.info("test_plot.pdf")$size
  )
  
  filesize = filesize/1000
  
  # cat("\n jpeg : " , round(file.info("test_plot.jpeg")$size/1000,0) )
  # cat("\n png  : " , round(file.info("test_plot.png")$size/1000, 0) )
  # cat("\n ps   : " , round(file.info("test_plot.ps")$size/1000,0) )
  # cat("\n pdf  : " , round(file.info("test_plot.pdf")$size/1000,0) )
  # cat("\n :", filesize , "\n")
  return(filesize)
  
}



#*******************************************************************************
# Q5. Diamonds
#
#
#
#*******************************************************************************
Q5 <-function() {
  
  ### Histogram - color, carat, price
  ggplot(diamonds, aes(x=carat)) + geom_histogram(binwidth = 0.2 ,color='black', fill='lightgreen') + 
    ggtitle(" Histogram - Carat") +
    scale_x_continuous(breaks=seq(0,3,0.4), limits=c(0,3)) 
   
  ggplot(diamonds, aes(x=price)) + geom_histogram(color='blue',fill='gray',binwidth=1000) + 
    scale_x_continuous(breaks=seq(0,20000,3000) )  +
    ggtitle(" Histogram - price")
  

  qplot(color, data = diamonds, geom = "bar",fill=color, main="Diamond count by color")  
  
  
  br = seq(0,20000,by=1000)
  ranges = paste(head(br,-1), br[-1], sep=" - ")
  freq = hist(diamonds$price, breaks = br, include.lowest=TRUE, plot=FALSE)
  data.frame(range = ranges, frequency = freq$counts)
  
  #hist price x axis aligned to non zero
  ggplot(diamonds, aes(x=price)) + geom_histogram(color='blue',fill='gray',binwidth=1000,center=500) + 
    ggtitle(" Histogram - price") +
    scale_x_continuous(breaks=seq(0,20000,4000)) +
    stat_bin(binwidth=1000,center=500, geom="text",  aes(label=..count..),size=3, vjust=-1, 
             position = position_dodge(width = 0.5) )
    
           
  x=table(cut(diamonds$price, breaks=seq(-500,20000,1000)))

    scale_x_continuous(breaks=seq(0,20000,3000) )  +
    ggtitle(" Histogram - price")
  
  #density function - carat & price
  
  ggplot(diamonds, aes(x=carat , y = ..density..)) + geom_histogram(binwidth = 0.1 , color='gray', fill='light blue') +
    geom_density(color='red', size=1) +
    ggtitle(" Density - Carat") +
    scale_x_continuous(breaks=seq(0,3,0.5) , limits=c(0,3))
  ggplot(diamonds, aes(x=(price), y = ..density..)) + geom_histogram(fill='lightblue',color = "gray", binwidth=1000) +
    scale_x_continuous(breaks=seq(0,20000,4000) )  +
    geom_density(color = 'red',size=1) +
    ggtitle(" Density - price")
  
  ggplot(diamonds, aes(x=log(carat) , y = ..density..)) + geom_histogram( color='gray', fill='light blue') +
    geom_density(color='blue', size=1) +
    ggtitle(" Density - log(Carat)") 
  ggplot(diamonds, aes(x=log(price), y = ..density..)) + geom_histogram(fill='lightblue',color = "gray") +
    geom_density(color = 'blue',size=1) +
    ggtitle(" Density - log(price)")

  
  # ggplot(diamonds, aes(x=(price))) + geom_density() + ggtitle("Density - Price")
  # ggplot(diamonds, aes(x=(carat))) + geom_density() + ggtitle("Density - Carat")
  # ggplot(diamonds, aes(x=log10(price))) + geom_density(color = 'blue',size=1) + ggtitle("Density - log(Price)")
  # ggplot(diamonds, aes(x=log10(carat))) + geom_density(color = 'blue',size=1) + ggtitle("Density - log(carat)")
  
  
  ### compute summary, kurtosis and skewness for price and carat
  summary(diamonds$price)
  summary(diamonds$carat)
  summary(diamonds$color)
  
  kurtosis(diamonds$price)
  kurtosis(diamonds$carat)
  skewness(diamonds$price)
  skewness(diamonds$price)
  
  
  ### Analyse  price vs. carat vs color relationship
  
  # plot price vs carat 
  qplot(carat, price, data = diamonds,alpha = I(1 /10)) + 
    ggtitle (" Diamonds - price vs. carat")
  
  qplot(carat, price, data = diamonds,alpha = I(1 /10) )+
    ggtitle (" Diamonds - log(price) vs. log(carat)") +
    scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 3,5)) +
    scale_y_log10(breaks = c(350, 1000, 5000, 10000, 15000,20000)) +
    xlab("log(carat)") + ylab("log(price)")
    stat_smooth(method='lm' )
  
  
  # plot price vs carat by color
  qplot(carat, price, data = diamonds,alpha = I(1 /10), color=color)+ 
      ggtitle( "Diamonds - price vs. carat by color") +
      facet_wrap(~color,scales="free_y") 
    
  qplot(carat, price, data = diamonds,alpha = I(1 /10), color=color)+ 
    ggtitle( "Diamonds - log(price) vs. log(carat)  by color") +
    scale_x_log10(breaks = c( 0.5, 1, 2, 3)) +
    scale_y_log10() +
    facet_wrap(~color) +
    geom_vline(xintercept=2, linetype='dotdash') +
    geom_hline(yintercept=10000, linetype='dotdash')
    
  
  cor.test(log(price), log(carat))
  cor.test(price,carat)
  
  
  #### sample data for ggpairs
  # set.seed(34546)
  # d_sample = diamonds[sample(1:length(price),1000),]
  # ggpairs(d_sample)
  
  
  # Price/Carat by color
  qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 /10))
  
  
  # boxplot : price by color
  qplot(color, price , data = diamonds, color= color, geom = "boxplot" ) +
    ggtitle("price by color")
  
  # boxplot : carat by color
  qplot(color, carat , data = diamonds, color= color, geom = "boxplot" ) +
    ggtitle("Carat by color")
  
  # boxplot : price/carat by color
  qplot(color, price/carat , data = diamonds, color= color, geom = "boxplot" ) +
    ggtitle("Price/carat by color")
    coord_cartesian(ylim=c(0,8000))
  
  
  # get price summary by color
  by(price,color,summary)

  #qplot(price, data=diamonds, geom='histogram', binwidth=1000, color=I('blue')) + facet_grid(color ~ .)
  
 # plot log(price) vs. log(carat) using color brewer for color
  qplot(carat,price, data = diamonds,alpha = I(.8), color=color) +
    scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 3,5)) +
    scale_y_log10(breaks = c(350, 1000, 5000, 10000, 15000,20000)) +
    scale_color_brewer("Color") +
    ggtitle("Log(Price) vs. Log(carat) by Color")
  
  # plot log(price) vs. log(carat) using color brewer for cut
  qplot(carat,price, data = diamonds,alpha = I(.8), color=cut) +
    scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 3,5)) +
    scale_y_log10(breaks = c(350, 1000, 5000, 10000, 15000,20000)) +
    scale_color_brewer("cut") +
    ggtitle("Log(Price) vs. Log(carat) by Cut")
  
  qplot(cut, price/carat , data = diamonds, color= cut, geom = "boxplot" ) +
    ggtitle("Price/carat by cut") +
    coord_cartesian(ylim=c(0,8000))
  
  # plot log(price) vs. log(carat) using color brewer for clarity
  qplot(carat,price, data = diamonds,alpha = I(1), color=Clarity) +
    scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 3,5)) +
    scale_y_log10(breaks = c(350, 1000, 5000, 10000, 15000,20000)) +
    scale_color_brewer("Color") +
    ggtitle("Log(Price) vs. Log(carat) by Clarity")
  
  qplot(clarity, price , data = diamonds, color= clarity, geom = "boxplot" ) +
    ggtitle("Price/carat by clarity") +
    coord_cartesian(ylim=c(0,8000))
  
  
  # df_temp = subset(diamonds, price/carat > 15000)
  # df_temp
  # dim(df_temp)
  # qplot(price/carat, data=df_temp,binwidth=1000) + facet_wrap(~cut , scales="free_y")
  
  # IQR ( Q3-Q1)
  temp =subset(diamonds,color=='J')
  IQR(temp$price)
  
  ## Digging deeper 
  ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point()
  ggplot(diamonds, aes(x=carat, y=price, color=color)) + geom_smooth(se=FALSE) +
    ggtitle("price vs. carat by color") +
    xlim(0,2.5)  
  
  tmp=subset(diamonds, (color=='D' | color=='E' | color=='F'))
  ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_smooth(se=FALSE) +
    ggtitle("price vs.carat by cut for color(D,E,F)")
  
  tmp2=subset(tmp, (cut != 'Fair'))
  ggplot(diamonds, aes(x=carat, y=price, color=clarity)) + geom_smooth(se=FALSE)+
    ggtitle("price vs.carat by clarity for color(D,E,F) and Cut!=Fair")
  
  
  # create a new variable volume
  diamonds$vol = x*y*z
  attach(diamonds)
  
  ##relation with x, y ,z
  p1=qplot(price,x, data=subset(diamonds, !is.na(x)) , main= "price vs. x") +
    scale_x_log10() +
    scale_y_continuous(limits=c(0, quantile(x,probs=.99))) 
  
  p2=qplot(price,y, data=subset(diamonds, !is.na(y)), main= "price vs. y") +
    scale_x_log10() +
    scale_y_continuous(limits=c(0, quantile(y,probs=.99))) 
  
  p3=qplot(price,z, data=subset(diamonds, !is.na(z)),main= "price vs. z") +
    scale_x_log10() +
    scale_y_continuous(limits=c(0, quantile(z,probs=.99))) 
  
  grid.arrange(p1,p2,p3,ncol=2)  
  
  # log(Price) vs. volume
  qplot(price,vol, data=subset(diamonds, !is.na(vol)) , main= "log(price) vs. log(volume)", alpha=I(.8)) +
    scale_x_log10() +
    scale_y_log10() +
    scale_y_continuous(limits=c(0, quantile(vol,probs=.99)))
  

  ## Build linear model
 
  m1<- lm(price ~ carat, data=diamonds)
  m2 <- update(m1 ,  ~ . + color)
  m3 <- update(m2 ,  ~ . + cut)
  m4 <- update(m3 , ~ . + clarity)
  mtable(m1,m2,m3,m4)

}




## test
# Q1a()
# Q1b()
# Q2()
 compare_filesize(50,80000,sample_cnt=10) 
#Q5()







