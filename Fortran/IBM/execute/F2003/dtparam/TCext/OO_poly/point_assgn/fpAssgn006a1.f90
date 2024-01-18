! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn006a1.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn006a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (pointer assignment for
!*                               pointer component may also take
!*                               place by execution of a derived type intrinsic
!*                               assignment statement; use unlimited
!*                               poly-pointer as the component)
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
    end type

    type, extends(base) :: child    ! (4,20)
        integer(k1) :: id = 1
    end type
end module

module m1
    type container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        class(*), pointer :: data => null()
    end type
end module

program fpAssgn006a1
use m
use m1

    type (container(4,20)) :: co, co1

    real*4, target :: f1
    type (base(4,20)), target :: b1
    type (child(4,20)), target :: c1
    class (base(4,:)), pointer :: b_ptr

    f1 = 10.0
    b1 = base(4,20)()
    c1 = child(4,20) (100)

    b_ptr => null()

    co%data => b_ptr
    co1 = co

    if (associated (co1%data) .or. associated (co%data)) error stop 1_4

    co%data => f1
    co1 = co

    if (.not. associated (co1%data, f1)) error stop 2_4

    co%data => b1
    co1 = co

    if (.not. associated (co1%data)) error stop 3_4

    if (associated (co1%data, b1)) error stop 10_4

    b_ptr => c1

    co%data => b_ptr
    co1 = co

    if (.not. associated (co1%data, c1)) error stop 4_4

end
