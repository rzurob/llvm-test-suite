! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg009a5.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/18/2005
!*
!*  DESCRIPTION                : argument association (VALUE attribute used in
!                               the type-bound)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1)    name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBaseValue (b)
        type(base(4)), value :: b

        print *, b%id
    end subroutine

    subroutine printChildValue (c)
        type(child(4,1,20)), value :: c

        print *, c%id, c%name
    end subroutine

    subroutine printBase (b)
        class (base(4)) :: b

        call printBaseValue (b)
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)) :: b

        call printChildValue (b)
    end subroutine
end module

program fArg009a5
use m
    class (base(4)), allocatable :: b1

    allocate (b1, source=base(4)(100))

    call b1%print

    deallocate (b1)

    allocate (b1, source=child(4,1,20)(-100, 'test 1'))

    call b1%print
end
