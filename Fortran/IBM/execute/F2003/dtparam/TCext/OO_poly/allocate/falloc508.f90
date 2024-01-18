! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc508.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (array section in source-expr)
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

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

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
end module

program falloc508
use m
    type (base(4)) :: b1 (10)
    class (base(4)), allocatable :: b2 (:)
    type (child(4,1,20)), target :: c1 (10)

    class (base(4)), pointer :: b3(:)

    b1 = (/(base(4) (i), i=10,1,-1)/)
    c1 = (/(child(4,1,20)(i, 'c1'), i=1,10)/)

    allocate (b2 (2:6), source=c1(::2))

    do i = 1, 5
        call b2(i+1)%print
        call c1(2*i-1)%print
    end do
end

