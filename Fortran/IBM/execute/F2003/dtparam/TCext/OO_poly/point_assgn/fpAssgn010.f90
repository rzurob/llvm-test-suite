! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/point_assgn/fpAssgn010.f
! opt variations: -qnok -ql

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn010.f
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
!*  DESCRIPTION                : pointer assignment (assignment caused the
!*                               pointer association status to be disassociated)
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
    end type

    type, extends(base) :: child    ! (4)
    end type
end module

program fpAssgn010
use m
    class (*), pointer :: x

    integer*4, pointer :: i_ptr => null()
    type(base(4)), pointer :: b_ptr
    type (child(4)), pointer :: cptr

    logical*8, pointer :: l_ptr

    l_ptr => null()
    x => i_ptr

    if (associated (x)) error stop 1_4

    b_ptr => null()

    x => b_ptr

    if (associated (x)) error stop 2_4

    cptr => null()

    x => cptr

    if (associated (x)) error stop 3_4

    x => l_ptr

    if (associated (x)) error stop 4_4
end
