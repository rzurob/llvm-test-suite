! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=self /tstdev/OO_poly/dummy_arg/fArg005a7.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg005a7.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited
!                               poly-allocatable dummy-arg only to be associated
!                               with unlimited poly-allocatable actual-arg)
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

    subroutine createData (x, x1)
        class (*), allocatable, intent(out) :: x
        class (*), intent(in) :: x1

        allocate (x, source=x1)
    end subroutine
end module

program fArg005a7
use m
use ISO_C_BINDING

    type seq1(n1,k1,k2)    ! (20,4,2)
        integer, kind :: k1,k2
        integer, len  :: n1
        sequence
        integer(k1)      i1
        integer(k2)      i2
        integer(k1)      i3
    end type

    type, BIND(C) :: bType
        integer(c_short) i1
        integer(c_int) i2
    end type

    type (seq1(:,4,2)), pointer :: s1
    type (bType), pointer :: b1

    class (*), allocatable, target :: x1, x2

    call createData (x1, seq1(20,4,2)(-1,-2,-3))

    call createData (x2, bType (-1, -10))

    s1 => x1
    b1 => x2

    if ((s1%i1 /= -1) .or. (s1%i2 /= -2) .or. (s1%i3 /= -3)) &
        error stop 1_4

    if ((b1%i1 /= -1) .or. (b1%i2 /= -10)) error stop 2_4
end
