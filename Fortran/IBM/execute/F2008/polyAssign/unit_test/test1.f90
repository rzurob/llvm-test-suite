Program test
    Type t
     integer :: i=1
    End Type
    Type,Extends(t) :: t2
     integer :: j=2
    End Type
    Class(t),allocatable:: x,y
    Class(t),pointer:: z
    !Type(t),Allocatable :: a
    !Type(t2),Allocatable :: b
    !x = t()
    !y = t2()
    !z = t()
    !allocate(a,b)
    allocate (x,y,source=t())
    allocate (z, source=t2())
    print *, y%i
    print *, x%i
    print *, z%i

End
