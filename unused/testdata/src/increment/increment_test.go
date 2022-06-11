package pkg

// Used because v2 is a sink
var v2 int //@ used_test(true)

func Bar() { //@ used_test(true)
	v2++
}
