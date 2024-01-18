!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass009_1.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 07/07/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : CLASS keyword (defined operator)
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
    type base
        integer(4) :: value
    end type

    interface operator (+)
        type (base) function b1AddI (b1, i)
        import base
            type (base), intent(in) :: b1
            integer*4, intent(in) :: i
        end function

        type (base) function iAddB1 (i, b1)
        import base
            integer*4, intent(in) :: i
            type (base), intent(in) :: b1
        end function

        type (base) function b1AddB2 (b1, b2)
        import base
            type (base), intent(in) :: b1, b2
        end function
    end interface
end module

program fclass009_1
use m
    type (base) :: b1, b2, b3

    b1 = base (10)
    b2 = base (20)

    b3 = b1 + 10 + ((3+b2) + (-1)) + 100 + b2 + b1 + 1

    if (b3%value /= 173) error stop 1_4

    b3 = b1 + b2 + b1 + (b2 + 1 + b2) + 10 + (1 * 20)

    if (b3%value /= 111) error stop 2_4
end

type (base) function b1AddI (b1, i)
use m, only: base
    type (base), intent(in) :: b1
    integer*4, intent(in) :: i

    b1AddI%value = b1%value + i
end function

type (base) function iAddB1 (i, b1)
use m, only: base
    integer*4, intent(in) :: i
    type (base), intent(in) :: b1

    iAddB1%value = b1%value + i
end function

type (base) function b1AddB2 (b1, b2)
use m, only : base
    type (base), intent(in) :: b1, b2

    b1AddB2%value = b1%value + b2%value
end function
