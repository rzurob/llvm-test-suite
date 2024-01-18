Program test
    Type t
     integer :: i=4
    End Type
    Class(t),allocatable:: x
    !allocate (x,source=t())
    x=t(5)
    print *, x%i

End
