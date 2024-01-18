! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg024.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg024.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (constraint C1233)
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

    subroutine test1 (b)
        class (base(4)), intent(inout), volatile :: b(:)
    end subroutine

    subroutine test2 (b)
        type (base(4)), intent(inout), volatile :: b(:)
    end subroutine
end module

program fArg024
use m
    class (base(4)), pointer :: b_ptr1(:)
    type (base(4)), pointer :: b_ptr2 (:)

    allocate (b_ptr1(10), b_ptr2(5))

    call test1 (b_ptr1)
    call test1 (b_ptr2)

    call test2 (b_ptr1)
    call test2 (b_ptr2)
end
