program main
  implicit none
  
  integer, allocatable :: i1(:,:), i2(:,:), src(:,:)
  
  allocate(src(2,4))
  allocate(i1, i2, source=src)
  allocate(i1, i2, mold=src)
end program
