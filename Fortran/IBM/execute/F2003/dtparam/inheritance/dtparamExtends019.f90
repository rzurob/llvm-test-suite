!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/29/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statement: An extended type has a scalar,
!                               nonpointer, nonallocatable parent component with
!                               the type and type parameters of the parent type.
!
!                               Case: test intrinsic assignments for the parent
!                               components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type complexArray (k, l)
        integer, kind :: k
        integer, len :: l

        complex(k) array(l)
        real(k), allocatable :: sum
        logical(k), pointer :: isSet
    end type

    type, extends(complexArray) :: namedCmplxArray (n)
        integer, len :: n

        character(n) name
    end type
end module


program dtparamExtends019
use m
    type(namedCmplxArray(8, 5, n=20)) nca(10)

    nca(1)%array = (/((i*1.0d0, i+10.0d0), i=1,5)/)
    allocate(nca(1)%sum, source=sum(real(nca(1)%array, 8)))
    allocate (nca(1)%isSet, source=.true._8)
    nca(1)%name = 'nca(1)'


    nca(2)%array = (/((i*2.0d0, i+2.0d1), i=1,5)/)
    allocate(nca(2)%sum)
    nca(2)%sum = sum(aimag(nca(2)%array))
    nullify(nca(2)%isSet)
    nca(2)%name = 'nca(2)'

    !! test the intrinsic assignment for the array sections
    nca(9:) = nca(2:1:-1)


    !! test the intrinsic assignment for the parent components
    nca(5:7:2)%complexArray = nca(1:2)%complexArray


    !! verify the results
    if ((.not. associated(nca(10)%isSet, nca(1)%isSet)) .or. &
        associated(nca(9)%isSet))   error stop 1_4

    do i = 10, 9, -1
        print *, 'element', i

        write (*, '(5("(",f6.1, ",", f6.1, ") "))') nca(i)%array

        write (*, '("sum = ", f12.2, "; name = ", a/)') nca(i)%sum, nca(i)%name
    end do


    do i = 5, 7, 2
        print *, 'element', i

        write (*, '(5("(",f6.1, ",", f6.1, ") "))') nca(i)%array
        write (*, '("sum = ", f12.2/)') nca(i)%sum
    end do
end
