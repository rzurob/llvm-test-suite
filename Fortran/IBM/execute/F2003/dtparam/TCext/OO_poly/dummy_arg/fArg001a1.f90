! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg001a1.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg001a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (keyword for function as
!*                               actual argument)
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
    end type

    contains

    logical function compare1 (b1, b2)
        class (base(4)), intent(in) :: b1, b2

        compare1 = (b1%id == b2%id)
    end function

    logical function compare2 (b1, b2)
        class (base(4)), intent(in) :: b1, b2

        compare2 = (b1%id < b2%id)
    end function
end module

module m1
use m
    contains

    logical function compareB1B2 (b1, b2, cmpFunc)
        class (base(4)), intent(in) :: b1, b2

        interface
            logical function cmpFunc (bb1, bb2)
            use m
                class (base(4)), intent(in) :: bb1, bb2
            end function
        end interface

        compareB1B2 = cmpFunc (b1, b2)
    end function
end module

program fArg001a1
use m1
    type (base(4)) :: b1, b2

    b1%id = 1
    b2%id = 2

    if (compareB1B2(b1, b2, cmpFunc = compare1)) error stop 1_4

    if (.not. compareB1B2 (b1= b2, cmpFunc = compare1, b2 = b2)) error stop 2_4

    if (.not. compareB1B2 (cmpFunc = compare2, b1=b1, b2=b2)) error stop 3_4

    if (compareB1B2 (b2, b1, compare2)) error stop 4_4
end
