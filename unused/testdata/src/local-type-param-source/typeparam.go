package tparamsource

// https://staticcheck.io/issues/1282

import (
	tparamsink "local-type-param-sink"
	"testing"
)

func TestFoo(t *testing.T) { //@ used(true)
	type EmptyStruct struct{} //@ used(true)
	_ = tparamsink.TypeOfType[EmptyStruct]()
}
