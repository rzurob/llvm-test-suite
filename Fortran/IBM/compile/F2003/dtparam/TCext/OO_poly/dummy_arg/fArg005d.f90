! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/07/2005
!*
!*  DESCRIPTION                : argument association (array element does not
!                               have POINTER/ALLOCATABLE attribute)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'no-name'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine createBase (b, id, name)
        class (base(4)), allocatable, intent(out) :: b
        integer*4, intent(in) :: id
        character(*), optional, intent(in) :: name

        if (present (name)) then
            allocate (b, source=child(4,1,20)(id, name))
        else
            allocate (b)
            b%id = id
        end if
    end subroutine

    subroutine printBasePtr (b)
        class (base(4)), pointer :: b

        if (associated (b)) call b%print
    end subroutine
end module

program fArg005d
use m
    class (base(4)), allocatable :: b1(:)

    class (base(4)), pointer :: b2(:,:)

    allocate (b1(2))
    allocate (b2 (10,10))

    call createBase (b1(2), 1, 'fail')  !<-- element does NOT have allocatable attr.

    call printBasePtr (b2(2, 3))        !<-- element does NOT have pointer attr.
end
