program main
  implicit none
 
  integer, parameter   :: i=-3
  logical, allocatable :: l
  complex, allocatable :: c
  
  allocate(c, l, source=i) 
end program
