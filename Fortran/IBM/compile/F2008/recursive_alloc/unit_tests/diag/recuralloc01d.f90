type dt
  integer i
  integer, allocatable :: q
  type(dt), allocatable :: p
  type(dt), allocatable :: r
end type
type(dt) x

x = dt(5, null())

x = dt(6, p=dt(3))

x = dt(7, null(), p=null())

end
