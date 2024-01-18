! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc001a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLCOATE (type-spec used in ALLOCATE statement;
!                               uses derived types)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2)    ! (4,20,4)
        integer, kind :: k2
        integer(k2)   :: id = -1

        contains

        procedure :: print => printChild
    end type

    type, extends(child) :: gen3(k3,n2)    ! (4,20,4,1,20)
        integer, kind             :: k3
        integer, len              :: n2
        character(kind=k3,len=n2) :: name = 'default'

        contains

        procedure :: print => printGen3
    end type

    contains

    subroutine printBase (b)
        class (base(4,*)), intent(in) :: b

        print *, 'empty type'
    end subroutine

    subroutine printChild (b)
        class (child(4,*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printGen3 (b)
        class (gen3(4,*,4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program falloc001a1
use m
    class (*), pointer :: x1

    class (base(4,20)), pointer :: b1
    class (base(4,20)), allocatable :: b2 (:)

    allocate (gen3(4,20,4,1,20) :: b1, x1, b2(2:3))

    call b1%print

    call b2(2)%print
    call b2(3)%print

    deallocate (b1, x1, b2)

    allocate (child(4,20,4) :: b2(1000:1001), x1, b1)

    call b1%print

    call b2(1000)%print
    call b2(1001)%print

    deallocate (x1, b1, b2)
end
