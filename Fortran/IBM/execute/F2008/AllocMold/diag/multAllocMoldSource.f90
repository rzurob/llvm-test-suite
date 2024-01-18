program main
  implicit none

  integer, parameter   :: src=-2
  integer, allocatable :: a, b

  allocate(a, b, mold=src, source=src)
end program main
