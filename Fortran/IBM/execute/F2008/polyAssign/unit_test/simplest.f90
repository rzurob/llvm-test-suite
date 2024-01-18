Program test
    Type t
     integer :: i=1
    End Type
    Type,Extends(t) :: t2
     integer :: j=2
    End Type
    Class(t),allocatable:: x
    allocate (x,source=t())
    !x=t()
    print *, x%i

End
