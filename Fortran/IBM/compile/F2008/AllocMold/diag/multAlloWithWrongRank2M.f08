program main
  implicit none

  real, allocatable :: r1(:), r2(:, :), src(:)

  allocate(src(4))
  allocate(r1, r2, mold=src)
end program
