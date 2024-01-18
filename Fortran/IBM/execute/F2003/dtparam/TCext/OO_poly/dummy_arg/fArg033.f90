! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg033.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg033.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument associated (rule 1 in section
!                               12.4.1.7: change of dummy-arg's value through
!                               actual arg)
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


    class (base(4)), pointer :: b1_m (:)
    type (base(4)), target :: c1 (3)

    contains

    subroutine test2 (b)
        class (base(4)), pointer :: b(:)

        b1_m => c1

        if (.not. associated (b, c1)) error stop 2_4
    end subroutine
end module

program fArg033
use m
    allocate (b1_m(20))

    call test1 (b1_m)


    call test2 (b1_m)

    contains

    subroutine test1 (b)
        class (base(4)), pointer :: b(:)

        deallocate (b1_m)

        if (associated (b)) error stop 1_4
    end subroutine
end
