! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/func_return/ffuncRet004.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly-function-return (nonpoly pointer
!                               function return in pointer assignment)
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

    type, extends(base) :: child    ! (4,20)
        integer(k1) :: id

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4,*)), intent(in) :: b

        print *, 'empty type'
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%id
    end subroutine

    type (child(4,20)) function produceChildPtr (id)
        pointer produceChildPtr
        integer*4, intent(in) :: id

        allocate (produceChildPtr)

        produceChildPtr%id = id
    end function
end module

program ffuncRet004
use m
    class (base(4,20)), pointer :: b

    b => produceChildPtr (10)

    call b%print

    deallocate (b)
end
