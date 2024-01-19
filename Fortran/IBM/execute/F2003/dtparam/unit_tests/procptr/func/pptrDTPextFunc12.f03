module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type Child(n2,k2)    ! (20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      ii
        integer(k2)      j
    end type
end module

module n
use m
    interface
        function func2(b)
        use m
          type(Base(*,4)) :: b(8)
          type(Child(20,4)), pointer :: func2(:)
        end function
    end interface

    type Container(k3,n3)    ! (4,20)
        integer, kind :: k3
        integer, len  :: n3
    end type
end module

program interfaceName001e
use n
    type(Child(20,4)) :: a(3)
    type(Base(20,4)) :: b(8)

    do m = 1, 8
      b(m)%i = m
    end do

    print *, b
    print *, func2(b)

    a = func2(b)
    print *, a
end

function func2(b)
use m
    type(Base(*,4)) :: b(8)
    type(Child(20,4)), pointer :: func2(:)
    allocate(func2(3))
    func2 = transfer(b, Child(20,4)(1,1), 3)
end function
