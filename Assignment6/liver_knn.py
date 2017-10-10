import numpy as np
from sklearn import preprocessing
from sklearn import cross_validation
from sklearn import neighbors
import pandas as pd

df = pd.read_csv('Indian Liver Patient Dataset (ILPD).csv')

df = df.fillna(method='ffill')

x = np.array(df.drop(['is_patient'], 1))

y = np.array(df['is_patient'])

inputX = []

for sample in x:
	temp = []
	for value in sample:
		if (value == "Female"):
			temp.append(0)
		elif (value == "Male"):
			temp.append(1)
		else:
			temp.append(value)
	inputX.append(temp)

# print(inputX)

inputY = [] 

for sample in y:
	if (sample == 1):
		inputY.append(0)
	else:
		inputY.append(1)

# print(inputY)

inputX = np.array(inputX)
inputY = np.array(inputY)

inputX_Train, inputX_Test, inputY_Train, inputY_Test = cross_validation.train_test_split(inputX, inputY, test_size=0.5)

classifier = neighbors.KNeighborsClassifier()
classifier.fit(inputX_Train, inputY_Train)
accuracy = classifier.score(inputX_Test, inputY_Test)
print(accuracy)