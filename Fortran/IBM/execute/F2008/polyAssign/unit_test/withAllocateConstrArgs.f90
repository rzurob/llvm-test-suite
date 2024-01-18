Program test
    Type t
     integer :: i=4
    End Type
    Class(t),allocatable:: x
    allocate (x,source=t(5)) 
    !x=t()
    print *, x%i
   
End
