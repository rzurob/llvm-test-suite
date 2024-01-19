Program test
    Type t
     integer :: i=4
    End Type
    Class(t),allocatable:: x
    x=t()
    print *, allocated(x)
    print *, x%i

End
