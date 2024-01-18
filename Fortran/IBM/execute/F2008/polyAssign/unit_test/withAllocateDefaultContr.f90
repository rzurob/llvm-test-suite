Program test
    Type t
     integer :: i=4
    End Type
    Class(t),allocatable:: x
    allocate (x, source=t())
    print *, x%i
End
