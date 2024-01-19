program main
  integer, pointer:: p1(:),p2(:),p3(:)
  integer, allocatable, target:: z(:)

  allocate(z(3))
  z=(/3,2,1/)
  p3=>z
  allocate(p1,p2,source=p3)

  print*, p1
  print*, p2
  print*, p3

end program main
