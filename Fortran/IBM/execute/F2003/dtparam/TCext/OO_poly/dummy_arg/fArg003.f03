! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg003.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (the temps created by
!*                               function calls used as actual arg)
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

        procedure :: add => produceBase
        procedure :: print => printBase
    end type

    contains

    type (base(4)) function produceBase (b, i)
        class (base(4)), intent(in) :: b
        integer*4, intent(in) :: i

        produceBase%id = b%id + i
    end function

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printVal1 (b)
        class (base(4)), intent(in) :: b

        call b%print
    end subroutine

    subroutine printVal2 (b)
        type (base(4)), intent(in) :: b

        call b%print
    end subroutine
end module

program fArg003
use m
    type (base(4)) :: b1

    b1 = base(4) (1)

    call printVal1 (b1%add (9))

    call printVal2 (b1%add (19))
end
