program main
  implicit none

  integer, allocatable :: n, isrc
  logical, allocatable :: l

  allocate(isrc)
  isrc = 3
  allocate(n, l, source=isrc)
end program