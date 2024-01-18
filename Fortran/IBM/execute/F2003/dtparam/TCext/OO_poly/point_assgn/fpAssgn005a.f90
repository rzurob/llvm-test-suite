! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/point_assgn/fpAssgn005a.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn005a.f
! %VERIFY: fpAssgn005a.out:fpAssgn005a.vf
! %STDIN:
! %STDOUT: fpAssgn005a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer's dynamic
!*                               types; use PASS binding to verify results)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id = 10

        contains

        procedure, pass :: print => printBase
    end type

    type, extends(base) :: child(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: name = ''

        contains

        procedure, pass :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, 'id = ', b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,4,*)), intent(in) :: b

        call printBase(b)

        print *, 'name = ', b%name
    end subroutine
end module

program fpAssgn005a
use m

    class (base(:,4)), pointer :: b_ptr
    class (child(20,4,4,20)), pointer :: c_ptr

    type (base(20,4)), target :: b1
    type (child(20,4,4,20)), target :: c1

    c1 = child(20,4,4,20) (20, name = 'c2')

    c_ptr => c1

    b_ptr => c_ptr%base

    call b_ptr%print  !! base type

    call c_ptr%print  !! child type

    b_ptr => c1%base

    call b_ptr%print  !! base type

    b_ptr => b1

    call b_ptr%print  !! base type

    allocate (c_ptr)

    c_ptr%id = 30
    c_ptr%name = 'c_ptr'

    b_ptr => c_ptr

    call b_ptr%print    !! child type

    b_ptr => c_ptr%base

    call b_ptr%print    !! base type

    b_ptr => c_ptr

    deallocate (b_ptr)
end
