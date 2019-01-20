# ########################
# ##Terminal
# #Install Homebrew 
# $ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
# 
# #Install python 3.6.5 through homebrew
# $brew install https://raw.githubusercontent.com/Homebrew/homebrew-core/f2a764ef944b1080be64bd88dca9a1d80130c558/Formula/python.rb
# 
# #intsall tensorflow & keras
# $pip3 install numpy
# $pip3 install pandas
# $pip3 install tensorflow
# $pip3 install keras
# $pip3 install scikit-learn
# ########################

#install reticulate
install.packages("reticulate")

#configuration python in R
reticulate::py_config()
reticulate::py_available()

#set default python version for R
reticulate::use_python("/usr/local/bin/python3")
##The R should be restarted after changing the python version used

#Check whether the tensorflow and keras are available
reticulate::py_module_available("tensorflow")
reticulate::py_module_available("keras")

#install tensorflow
install.packages("tensorflow")

#test tensorflow
sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)

#install keras
install.packages("keras")

#Test keras
mnist <- keras:: dataset_mnist()
