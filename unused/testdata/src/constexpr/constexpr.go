package pkg

import (
	"io"
	"unsafe"
)

// https://staticcheck.io/issues/812

var (
	w  io.Writer          //@ used(true)
	sz = unsafe.Sizeof(w) //@ used(true)
)

var _ = sz
