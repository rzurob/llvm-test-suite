!!! Argument of STORAGE_SIZE must be a data object. It cannot be an expression,
!!! structure constructor, array constructor, procedure call that is not 
!!! variable.
integer i, j
type dt
  real r1
end type
i = storage_size(1+3)
i = storage_size(j+3)
i = storage_size(dt(1.0))
i = storage_size([1,2])
i = storage_size(foo())
contains
function foo()
integer foo
foo = 2
end
end

