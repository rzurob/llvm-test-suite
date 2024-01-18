! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn007a1.f
! opt variations: -qnok -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn007a1.f
! %VERIFY: fpAssgn007a1.out:fpAssgn007a1.vf
! %STDIN:
! %STDOUT: fpAssgn007a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (nopass binding can be
!*                               called for poly pointer; associated or
!*                               disassociated)
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
        contains

        procedure, nopass :: printType => printBaseType
    end type

    type, extends(base) :: child(k2,n2)    ! (4,20,4,20)
        integer, kind :: k2
        integer, len  :: n2
        contains

        procedure, nopass :: printType => printChildType
    end type

    contains

    subroutine printBaseType
        print *, 'base'
    end subroutine

    subroutine printChildType
        print *, 'child'
    end subroutine
end module

program fpAssgn007a1
use m
    type (base(4,:)), pointer :: p1

    type (child(4,20,4,20)), pointer :: c_ptr
    type (child(4,20,4,20)), target :: c1

    class (base(4,:)), pointer :: b_ptr
    class (child(4,20,4,20)), pointer :: c_ptr1

    call p1%printType       !<-- base(4,20)

    b_ptr => c1

    call b_ptr%printType    !<-- child(4,20,4,20)

    p1 => b_ptr

    call p1%printType       !<-- base(4,20)

    allocate (c_ptr1)

    b_ptr => c_ptr1

    call b_ptr%printType    !<-- child(4,20,4,20)

    deallocate (c_ptr1)

    b_ptr => c_ptr1

    call b_ptr%printType    !<-- base(4,20)

    allocate (c_ptr)

    deallocate (c_ptr)

    b_ptr => c_ptr

    call b_ptr%printType    !<-- base(4,20)
end

