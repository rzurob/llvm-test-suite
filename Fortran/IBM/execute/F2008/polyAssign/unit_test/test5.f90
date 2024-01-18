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
  x=t1()

  select type (x)
    type is (t)
      print *, "type is t"
      print *, x%i
      print *, x%j
    type is (t1)
      print *, "type is t1"
  end select 
end program
