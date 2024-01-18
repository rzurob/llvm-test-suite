Program test
    Type t
     integer :: i=4
    End Type
    Class(*),allocatable:: x
    allocate (x,source=t())
End
