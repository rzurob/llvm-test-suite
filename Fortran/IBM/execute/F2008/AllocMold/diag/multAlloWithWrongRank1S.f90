program main
  implicit none
  
  integer, allocatable :: i1(:), i2(:,:), src(:,:)
  
  allocate(src(2,4))
  allocate(i2, i1, source=src)
end program
