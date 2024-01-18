program main
  implicit none

  integer, allocatable :: i(:), isrc(:)

  allocate(isrc(9))
  allocate(i, i, source=isrc)
end program
