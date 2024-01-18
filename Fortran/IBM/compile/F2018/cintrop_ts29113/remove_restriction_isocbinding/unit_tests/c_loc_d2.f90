use, intrinsic :: iso_c_binding
type dt (k)
  integer, len :: k
  real :: r1(k)
end type

type(c_ptr) :: addr

type(dt(4)), target :: d1

addr = c_loc(d1)

end
  
