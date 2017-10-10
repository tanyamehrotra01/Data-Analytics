import tflearn
import numpy as np
from sklearn import preprocessing
from sklearn import cross_validation
import pandas as pd

df = pd.read_csv('Indian Liver Patient Dataset (ILPD).csv')

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

inputY = [] 

for sample in y:
	if (sample == 1):
		inputY.append([1, 0])
	else:
		inputY.append([0, 1])

net = tflearn.input_data(shape=[None, len(inputX[0])])
net = tflearn.fully_connected(net, 13)
net = tflearn.fully_connected(net, 17)
net = tflearn.fully_connected(net, 2, activation='sigmoid')
net = tflearn.regression(net, optimizer = 'Nesterov')

model = tflearn.DNN(net)

model.fit(inputX, inputY, n_epoch = 10, validation_set = 0.1, batch_size = 64, show_metric=True, run_id='Liver')

model.save("Liver_NN.model")