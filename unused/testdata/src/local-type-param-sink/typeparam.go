package tparamsource

// https://staticcheck.io/issues/1282

import "reflect"

func TypeOfType[T any]() reflect.Type { //@ used(true)
	var t *T
	return reflect.TypeOf(t).Elem()
}
