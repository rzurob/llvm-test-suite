! GB DTP extension using:
! ftcx_dtp -qck -qnol -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn026.f
! opt variations: -qnock -ql -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer as
!*                               dummy-arg)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure, pass :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure, pass :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)) :: b

        print *, 'id = ', b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)) :: b

        call b%base%print
        print *, 'name = ', b%name
    end subroutine
end module

program fpAssgn026
use m

    class (base(4)), pointer :: x(:)
    type (child(4,1,20)), pointer :: c1(:)

    allocate (c1(3))

    c1 = (/(child(4,1,20) (i, 'no-name'), i=1,3)/)

    x => c1

    call abc(x)

    contains

    subroutine abc (b)
        class (base(4)), pointer :: b(:)

        do i = 1, size(b)
            call b(i)%print
        end do

        deallocate(b)
    end subroutine
end

