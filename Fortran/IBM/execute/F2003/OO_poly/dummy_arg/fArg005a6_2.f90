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
! %GROUP: fArg005a6_2.f
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
!*  DATE                       : 06/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (unlimited poly-pointer
!                               dummy-arg to be associated only with unlimited
!                               poly-pointer actual-arg; use of sequence type
!                               and the BIND(C) types to verify the data
!                               creation)
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
    contains

    subroutine createObj (x, x1)
        class (*), pointer, intent(out) :: x
        class (*), intent(in) :: x1

        allocate (x, source=x1)
    end subroutine
end module

program fArg005a6_2
use m
use ISO_C_BINDING

    class (*), pointer :: x => null()
    class (*), pointer :: x1 => null()

    type seq1
        sequence

        integer(4) i1
        integer(8) i2
        integer(2) i3
    end type

    type, bind(c) :: bType
        integer(C_INT) i1
        integer(C_SHORT) i2
    end type

    type (seq1), pointer :: s1

    type (bType), pointer :: b1

    call createObj (x, seq1(1,2,3))

    s1 => x

    if ((s1%i1 /= 1) .or. (s1%i2 /= 2) .or. (s1%i3 /= 3)) error stop 1_4

    call createObj (x1, bType (100, -1))

    b1 => x1

    if ((b1%i1 /= 100) .or. (b1%i2 /= -1)) error stop 2_4
end
