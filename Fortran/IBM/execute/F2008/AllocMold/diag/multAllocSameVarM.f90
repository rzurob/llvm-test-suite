program main
  implicit none
  
  integer, allocatable :: i(:), isrc(:)
  
  allocate(isrc(9))
  allocate(i, i, mold=isrc)
end program
