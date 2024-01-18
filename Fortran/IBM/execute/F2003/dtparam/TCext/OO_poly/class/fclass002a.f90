! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/class/fclass002a.f
! opt variations: -qck -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : class (argument association,
!*                               poly-dummy-arg-array; resolve pass type-bounds)
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
        integer(k1)   :: id = 0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name = ''

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fclass002a
use m

    type (base(4)) :: b1(2:4)
    type (child(4,20)) :: c1(0:3)

    b1 = (/(base(4) (i*10), i=2,4)/)

    c1 = (/(child(4,20)(i+1, name='c1'), i=0,3)/)

    call abc (b1)

    call abc (c1)

    contains

    subroutine abc (a)
        class (base(4)), intent(in) :: a(:)

        do i = 1, size(a)
            call a(i)%print
        end do
    end subroutine
end
