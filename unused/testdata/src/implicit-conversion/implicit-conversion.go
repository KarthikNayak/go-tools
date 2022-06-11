package pkg

// https://staticcheck.io/issues/810

type Thing struct { //@ used(true)
	has struct { //@ used(true)
		a bool //@ used(true)
	}
}

func Fn() { //@ used(true)
	type temp struct { //@ used(true)
		a bool //@ used(true)
	}

	x := Thing{
		has: temp{true},
	}
	_ = x
}
