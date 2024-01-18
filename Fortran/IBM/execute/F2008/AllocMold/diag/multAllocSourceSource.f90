program main
  implicit none
  
  logical, parameter   :: src=.false.
  logical, allocatable :: a, b

  allocate(a, b, source=src, source=src)
end program main
