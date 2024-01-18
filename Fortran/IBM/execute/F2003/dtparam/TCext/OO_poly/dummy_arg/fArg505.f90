! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg505.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg505.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-pointer-dummy-arg;
!*                               a basic test)
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
        integer(k1)   :: id = -1
    end type

    contains

    subroutine createArray (b1, i)
        class (base(4)), pointer, intent(out) :: b1(:)
        integer*4, intent(in) :: i

        allocate (b1(i))
    end subroutine
end module


program fArg505
use m
    class (base(4)), pointer :: x(:) => null()


    call createArray (x, 10)

    if (.not. associated (x)) error stop 1_4

    if (size(x) /= 10) error stop 2_4

    if (any (x%id /= -1)) error stop 3_4

    deallocate (x)
end
