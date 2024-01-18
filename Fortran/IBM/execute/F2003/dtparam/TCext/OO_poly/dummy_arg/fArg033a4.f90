! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg033a4.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg033a4.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (changes done through
!                               actual-arg or dummy-arg can be seen by the
!                               couter-part; no use of register or cache memory)
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
        class(*), pointer :: data => null()
    end type

    class (base(4,:)), pointer :: b1_m

    integer*4, target :: i1, i2

    contains

    !! call this routine with b1_m as actual-arg with component data is nullified
    subroutine test1 (b)
        class (base(4,*)), target, intent(inout) :: b

        if (associated (b%data)) error stop 1_4

        b1_m%data => i1

        if (.not. associated (b%data, i1)) error stop 2_4

        b%data => i2

        if (.not. associated (b1_m%data, i2)) error stop 3_4
    end subroutine
end module

program fArg033a4
use m
    type (base(4,20)), target :: b1

    b1_m => b1

    call test1 (b1_m)
end
