! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg516.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (OPTIONAL attribute allowed
!                               for passed-object dummy-arg)
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

        procedure :: print => printVal
    end type

    contains

    subroutine printVal (b)
        class (base(4)), optional, intent(in) :: b

        if (present (b)) then
            print *, b%id
        else
            print *, 'returning'
        end if
    end subroutine
end module

program fArg516
use m
    type (base(4)) :: b1 = base(4) (100)

    call b1%print

    call printVal ()
end
