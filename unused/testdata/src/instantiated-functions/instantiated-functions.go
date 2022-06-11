package pkg

// https://staticcheck.io/issues/1199

type c1 struct{} //@ used(false)

func Fn[T any]() {} //@ used(true)

func uncalled() { //@ used(false)
	Fn[c1]()
}
