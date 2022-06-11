package pkg

var x int //@ used(true)

func Foo() { //@ used(true)
	var s []int
	s[x] = 0
}
