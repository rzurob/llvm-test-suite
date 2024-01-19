Program test
    Type t
     integer :: i=1
    End Type
    Type,Extends(t) :: t2
     integer :: j=2
    End Type

    Class(*),target,allocatable:: x
    Class(*),pointer:: px
    x = t2(5,7)
    !allocate (x,source=t())

    px=>x
    select type(px)
    type is (t)
      print *, "type of x is t"
      print *, px%i
    class is (t)
      print *, "class of x is t"
      print *, px%i
    type is (t2)
      print *, "type of x is t2"
      print *, px%j
    class is (t2)
      print *, "class of x is t2"
      print *, px%j
    end select
End
