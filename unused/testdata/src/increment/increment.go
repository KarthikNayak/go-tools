package pkg

type T struct { //@ used(true), used_test(true)
	// Writing to fields uses them
	f int //@ used(true), used_test(true)
}

// Not used, v is only written to
var v int //@ used(false), used_test(false)

func Foo() { //@ used(true), used_test(true)
	var x T
	x.f++
	v++
}
