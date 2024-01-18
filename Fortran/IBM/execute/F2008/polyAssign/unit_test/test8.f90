Program test
    Type t
     integer :: i=1
    End Type
    Type,Extends(t) :: t2
     integer :: j=2
    End Type
    Class(t),allocatable:: x,y
    !Class(t),pointer:: z
    Type(t),Allocatable :: a
    !z = t()
    x = t(3)
    y = t2(2)
    x = t2(10)
    !allocate(a)
    !allocate (x,y,source=a)
    print *, y%i
    print *, x%i
    !print *, z%i

End
