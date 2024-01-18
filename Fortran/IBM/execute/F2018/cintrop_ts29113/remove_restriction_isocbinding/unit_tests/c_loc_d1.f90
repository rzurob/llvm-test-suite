use, intrinsic :: iso_c_binding
type dt (k)
  integer, kind :: k
  real :: r1(k)
end type

type(c_ptr) :: addr

type(dt(4)), target :: d1(4)

addr = c_loc(d1)

end
  
