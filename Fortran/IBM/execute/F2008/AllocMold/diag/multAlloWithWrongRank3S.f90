program main
  implicit none
  
  character, allocatable :: c1(:), c2(:, :), src(:)
  
  allocate(src(9))
  allocate(c2, c1, source=src)
end program
