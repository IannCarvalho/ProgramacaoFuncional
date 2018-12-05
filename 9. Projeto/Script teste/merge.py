todosOsArrays = [[],[15], [9,3,7,1,8,12,10,20,15,18], [5,10, 30, 40, 28], [6,11,8,1,14,12,2,7,0,18,10,5], [8,7,3,20,2,17,5,21,11,12], [4,8,7,11,10,5], [7,15,12,4,10,1,5,13,6,14,11,3,9,0,2,8], [1,2,5,7,9,10]]
allValues = []

answer = ''
for i in range(0, len(todosOsArrays)):
	y = 1;
	for j in range(i+1, len(todosOsArrays)):
		answer = answer + 'T.TestCase (T.assertEqual "mergeMax' + str(i+1) + '.' + str(y) + '" (' + str(todosOsArrays[i]) +  ' ++ ' + str(todosOsArrays[j]) + ') (CT.order(CT.mergeMaxCT (CT.buildMaxCT ' + str(todosOsArrays[i]) + ' ) (CT.buildMaxCT ' + str(todosOsArrays[j]) + ')))),\n'
		y += 1
		
for i in range(0, len(todosOsArrays)):
	y = 1;
	for j in range(i+1, len(todosOsArrays)):
		answer = answer + 'T.TestCase (T.assertEqual "mergeMin' + str(i+1) + '.' + str(y) + '" (' + str(todosOsArrays[i]) +  ' ++ ' + str(todosOsArrays[j]) + ') (CT.order(CT.mergeMinCT (CT.buildMinCT ' + str(todosOsArrays[i]) + ' ) (CT.buildMinCT ' + str(todosOsArrays[j]) + ')))),\n'
		y += 1

print answer
