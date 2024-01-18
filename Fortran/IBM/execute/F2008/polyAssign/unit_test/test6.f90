program main
  Type t
    integer:: i=0
    integer:: j=1
  end type

  Type t1
    integer:: k=2
  end type

  class (*), allocatable :: x
  x=t()
  if (allocated(x)) then
    deallocate(x)
  end if
  x=t1()

end program
