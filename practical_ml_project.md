Download and read data.
-----------------------

### Load both training and testing data into R and specify the missing values. Check the dimensions of column names of both datasets.

    training<-read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",na.strings=c("NA",""))
    testing<-read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",na.strings=c("NA",""))
    dim(training)

    ## [1] 19622   160

    dim(testing)

    ## [1]  20 160

    names(training)
    names(testing)

Preprocess
----------

### First, we need to make sure that training and testing datasets have identical column names.

    all.equal(colnames(training)[1:dim(training)[2]-1],colnames(testing)[1:dim(testing)[2]-1])

    ## [1] TRUE

### According to the study , the first seven variables are irrelevant to the current analysis. Therefore, I remove the first seven variables from the original dataset.

    trainingSub<-training[,-c(1:7)]
    testingSub<-testing[,-c(1:7)]

### Divide the original training dataset further into one training and one testing dataset. This new training data will be used to train the algorithm which will then be applied to the new testing data.

    library(caret)

    ## Warning: package 'caret' was built under R version 3.2.3

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.2.3

    indexTrain<-createDataPartition(y=trainingSub$classe,p=0.6,list=FALSE)
    mytrain<-trainingSub[indexTrain,]
    mytest<-trainingSub[-indexTrain,]
    dim(mytrain)

    ## [1] 11776   153

    dim(mytest)

    ## [1] 7846  153

### Remove the columns with missing values from the original training dataset.

    mytrainCmplt<-mytrain[,colSums(is.na(mytrain))==0]
    mytestCmplt<-mytest[,names(mytest) %in% names(mytrainCmplt)]
    testingCmplt<-testingSub[,names(testingSub) %in% names(mytrainCmplt)]
    testingCmplt<-cbind(testingCmplt,testingSub[,153])
    colnames(testingCmplt)[53]<-"classe"

### Check to see whether there are any variables that have variance close to zero. If so, remove them from the training dataset. The following results demonstrate there are no such variables in the training dataset.

    nearzv<-nearZeroVar(mytrainCmplt,saveMetrics=TRUE)
    sum(nearzv$nzv)

    ## [1] 0

Evaluation
----------

### Classification tree

    set.seed(123)
    modFit1<-train(classe~.,method="rpart",data=mytrainCmplt)

    ## Loading required package: rpart

    print(modFit1$finalModel)

    ## n= 11776 
    ## 
    ## node), split, n, loss, yval, (yprob)
    ##       * denotes terminal node
    ## 
    ##  1) root 11776 8428 A (0.28 0.19 0.17 0.16 0.18)  
    ##    2) roll_belt< 129.5 10728 7418 A (0.31 0.21 0.19 0.18 0.11)  
    ##      4) pitch_forearm< -33.95 962    4 A (1 0.0042 0 0 0) *
    ##      5) pitch_forearm>=-33.95 9766 7414 A (0.24 0.23 0.21 0.2 0.12)  
    ##       10) magnet_dumbbell_y< 436.5 8232 5942 A (0.28 0.18 0.24 0.19 0.11)  
    ##         20) roll_forearm< 123.5 5126 3065 A (0.4 0.18 0.19 0.17 0.057) *
    ##         21) roll_forearm>=123.5 3106 2079 C (0.074 0.18 0.33 0.23 0.19) *
    ##       11) magnet_dumbbell_y>=436.5 1534  750 B (0.04 0.51 0.042 0.23 0.18) *
    ##    3) roll_belt>=129.5 1048   38 E (0.036 0 0 0 0.96) *

    predtree<-predict(modFit1,newdata=mytestCmplt)
    confusionMatrix(predtree,mytestCmplt$classe)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1990  637  623  561  195
    ##          B   33  518   45  234  196
    ##          C  173  363  700  491  362
    ##          D    0    0    0    0    0
    ##          E   36    0    0    0  689
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.4967          
    ##                  95% CI : (0.4856, 0.5078)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.343           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.8916  0.34124  0.51170   0.0000  0.47781
    ## Specificity            0.6409  0.91972  0.78558   1.0000  0.99438
    ## Pos Pred Value         0.4968  0.50487  0.33509      NaN  0.95034
    ## Neg Pred Value         0.9370  0.85337  0.88397   0.8361  0.89426
    ## Prevalence             0.2845  0.19347  0.17436   0.1639  0.18379
    ## Detection Rate         0.2536  0.06602  0.08922   0.0000  0.08782
    ## Detection Prevalence   0.5106  0.13077  0.26625   0.0000  0.09240
    ## Balanced Accuracy      0.7662  0.63048  0.64864   0.5000  0.73609

The accuracy is low around 0.5 suggesting classification tree is not a
desirable algorithm in this case.

### Random forest

    library(randomForest)

    ## Warning: package 'randomForest' was built under R version 3.2.3

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    set.seed(234)
    modFit2<-randomForest(classe~.,data=mytrainCmplt)
    predrf<-predict(modFit2,newdata=mytestCmplt,type="class")
    confusionMatrix(predrf,mytestCmplt$classe)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2232    6    0    0    0
    ##          B    0 1512    6    0    0
    ##          C    0    0 1361   18    1
    ##          D    0    0    1 1267    2
    ##          E    0    0    0    1 1439
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9955          
    ##                  95% CI : (0.9938, 0.9969)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9944          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   0.9960   0.9949   0.9852   0.9979
    ## Specificity            0.9989   0.9991   0.9971   0.9995   0.9998
    ## Pos Pred Value         0.9973   0.9960   0.9862   0.9976   0.9993
    ## Neg Pred Value         1.0000   0.9991   0.9989   0.9971   0.9995
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2845   0.1927   0.1735   0.1615   0.1834
    ## Detection Prevalence   0.2852   0.1935   0.1759   0.1619   0.1835
    ## Balanced Accuracy      0.9995   0.9975   0.9960   0.9924   0.9989

Using random forest, I obtain a high accuracy of classification.
Therefore, I prefer random forest to classification tree. I applied
random forest to the testing dataset provided by Professor Leek and
correctly predicted all the classifications.
