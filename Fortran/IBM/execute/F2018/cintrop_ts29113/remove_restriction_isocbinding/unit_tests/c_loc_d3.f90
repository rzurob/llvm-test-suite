use, intrinsic :: iso_c_binding
type dt (k)
  integer, kind :: k
  real :: r1(k)
end type

type(c_ptr) :: addr

type(dt(4)), pointer :: d1(:)

allocate(dt(4) :: d1(4))
addr = c_loc(d1)

end

