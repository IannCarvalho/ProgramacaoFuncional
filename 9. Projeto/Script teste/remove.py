todosOsArrays = [[],[15], [9,3,7,1,8,12,10,20,15,18], [5,10, 30, 40, 28], [6,11,8,1,14,12,2,7,0,18,10,5], [8,7,3,20,2,17,5,21,11,12], [4,8,7,11,10,5], [7,15,12,4,10,1,5,13,6,14,11,3,9,0,2,8], [1,2,5,7,9,10]]

answer = ''
j = 0;
for array in todosOsArrays:
	for i in range(len(array)):
		answer = answer + '    T.TestCase (T.assertEqual "removeMax' + str(j) + '.' + str(i+1) + '" (DL.delete ' + str(array[i]) + ' ' + str(array) + ') (CT.order(CT.removeMaxCT ' + str(i) + ' (CT.buildMaxCT ' + str(array) + ')))),\n'
	j += 1;

j = 0;
for array in todosOsArrays:
	for i in range(len(array)):
		answer = answer + '    T.TestCase (T.assertEqual "removeMin' + str(j) + '.' + str(i+1) + '" (DL.delete ' + str(array[i]) + ' ' + str(array) + ') (CT.order(CT.removeMinCT ' + str(i) + ' (CT.buildMinCT ' + str(array) + ')))),\n'
	j += 1;
	
print answer
