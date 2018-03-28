# Cardiac-Risk-Machine-Learning-Prediction APp
Shiny App classifier predicting cardiac risk. Project completed during MSc Digital Health Karolinska Institutet 
https://healthdiggers.shinyapps.io/cdss_app/

# Model Development 
Following initial assessment of 12 classification models, the cross-validation decision tree pre-test attributes model was chosen as our model of interest. It had the best recall performance (83%), our metric of high importance, and the best F-measures (77%) of the 12 models. The iterative improvement phase then compared and contrasted four changes made to this model. Performance showed that a model with: Gini index impurity weighting, chest pain as a binomial attribute, a threshold value for trestbps, and removing chol and fbs attributes resulted in the best model. Therefore, it includes the attributes: age, sex, cp, trestbps and restecg.
Finally, this final model has satisfactory performance but poor applicability to new data. Testing was carried out using data from the same dataset, not new instances from the real-world. It would require further improvement with current diagnostic practices and new data before consideration for implementation.
In light of increasing quality of care and minimizing unnecessary costs of testing, we advise different action steps along with the predicted outcome. High-value care must consider the varying inherent disease likelihoods of our subgroups. Asymptomatic chest pain patients predicted with disease are recommended directly for cardiac catheterization. The inherently high prior probability risk (74%) and low gain (3%) in posterior probability warrants this action. Symptomatic chest pain patients predicted with disease are first recommended for CCTA since the posterior probability is significantly lower (46%). Symptomatic chest pain patients predicted without disease are recommended for exercise stress test  with or without ECG. 

# Shiny App
The model is built using the Caret package for training the Decision Tree. The Rpart package is used for training the Decision Tree The training uses the rpart package that handles recursive partitioning.
The prediction is done using the predict function from the stats package which accepts a wide array of models to be used in conjunction with a dataset of attributes to predict probabilities of a classification.

# Data
UCI Heart Disease dataset available at the UCI Machine Learning data repository: http://archive.ics.uci.edu/ml/datasets/heart+Disease
The original authors of the UCI databases
Hungarian Institute of Cardiology. Budapest: Andras Janosi, M.D.
University Hospital, Zurich, Switzerland: William Steinbrunn, M.D.
University Hospital, Basel, Switzerland: Matthias Pfisterer, M.D.
V.A. Medical Center, Long Beach and Cleveland Clinic Foundation:Robert Detrano, M.D., Ph.D.


Contributors include: Per Nyberg, Loes Crielaard, Clàudia Figueras Julián, Erik Scalfaro
