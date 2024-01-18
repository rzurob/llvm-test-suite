program main
  implicit none

  integer, parameter   :: src=-2
  integer, allocatable :: a, b

  allocate(a, b, source=src, mold=src)
end program main
