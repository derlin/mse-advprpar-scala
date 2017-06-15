//def target = 612

def threshold = 0.00001  // precision threshold (stop condition)

// derivative and function for sqrt
def deriv2(x: Double) = 2 * x
def fn2(target: Double, x: Double) = x * x - target

// the sqrt itself
def sqrt(target: Double, x0: Double = 10): Double = if (math.abs(fn2(target, x0)) < threshold) x0
else sqrt(target, x0 - (fn2(target, x0) / deriv2(x0)))

sqrt(612)
sqrt(2)
sqrt(4)

// the same for the cubig root

def deriv3(x: Double) = 3 * x * x
def fn3(target: Double, x: Double) = x * x * x - target

def sqrt3(target: Double, x0: Double = 10): Double = if (math.abs(fn3(target, x0)) < threshold) x0
else sqrt3(target, x0 - (fn3(target, x0) / deriv3(x0)))


sqrt3(27)
