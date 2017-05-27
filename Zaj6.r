library("neuralnet")

#Going to create a neural network to perform prediction
#Type ?neuralnet for more information on the neuralnet library

#Generate training data
#And store them as a dataframe
traininginput <- as.data.frame(matrix(c(276, 130, 17,
                                        262, 98, 18,
                                        196, 75, 28,
                                        192, 98, 21,
                                        206, 98, 24,
                                        228, 111, 24,
                                        188, 98, 20,
                                        279, 87, 16,
                                        206, 98, 24,
                                        246, 98, 16,
                                        100, 56, 20,
                                        210, 98, 20,
                                        212, 98, 18,
                                        361, 211, 12,
                                        166, 64, 19), nrow=15, ncol=3))
trainingoutput <- c(3399, 2099, 899, 1399, 1799, 1199, 1699, 1999, 2199, 2099, 890, 1899, 1799, 3449, 1198)

#Column bind the data into one variable
trainingdata <- cbind(traininginput, trainingoutput)

# Create Vector of Column Max and Min Values
maxs <- apply(trainingdata[,], 2, max)
mins <- apply(trainingdata[,], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.trainingdata <- as.data.frame(scale(trainingdata[,], center=mins, scale=maxs-mins))
trainingdata <- scaled.trainingdata

# Check out results
print(head(trainingdata, 10))

colnames(trainingdata) <- c("Cooler_storage_volume", "Freezer_storage_volume", "Temp_keep_in_h_if_power_outage", "Price") 
print(trainingdata)

#Train the neural network
#Going to have C(6, 5, 3) hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.price <- neuralnet(Price~Cooler_storage_volume+Freezer_storage_volume+Temp_keep_in_h_if_power_outage, trainingdata, hidden=c(6, 5, 3), threshold=0.001)
print(net.price)

#Plot the neural network
plot(net.price)

#Test the neural network on some training data
testdata <- as.data.frame(matrix(c(262, 64, 28,
                                   206, 75, 16,
                                   361, 56, 20), nrow=3, ncol=3))
scaled.testdata <- as.data.frame(scale(testdata[,], center=mins[1:3], scale=maxs[1:3]-mins[1:3]))
net.results <- compute(net.price, scaled.testdata) #Run them through the neural network

#Lets see what properties net.price has
ls(net.results)

#Lets see the results
print(net.results$net.result)
