


def some(func, array):
    if len(array) == 0:
	return False

    for i in range(len(array)):
	result = func(array[i])
	if result:
	    return result
    return False
	
def every(func, array):
    if len(array) == 0:
	return True

    for i in range(len(array)):
	result = func(array[i])
	if not result:
	    return False
    return True
