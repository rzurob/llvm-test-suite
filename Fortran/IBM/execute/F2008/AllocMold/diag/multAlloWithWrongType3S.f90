program main
  implicit none
  
  integer, allocatable :: i, isrc
  complex, allocatable :: c
  
  allocate(isrc)
  isrc = -9
  allocate(c, i, source=isrc)
end program
