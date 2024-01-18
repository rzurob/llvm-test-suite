! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv /tstdev/OO_poly/point_assgn/fpAssgn001aa1.f
! opt variations: -qnol -qdefaultpv

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn001aa1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (test associated() on
!*                               unlimited poly-pointer which is assigned to
!*                               structure component; it'll return TRUE as there
!*                               is no check for dynamic types, only the
!*                               occupences' addresses)
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
    type A(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type B(n2,k2)    ! (20,4)
        integer, kind  :: k2
        integer, len   :: n2
        type(A(n2,k2))    a1
    end type
end module

program fpAssgn001aa1
use m
    class(*), pointer :: i_ptr
    class(*), pointer :: a_ptr

    type (A(20,4)), target :: aa
    type (B(20,4)), target :: bb

    i_ptr => aa%i

    a_ptr => bb%a1

    if ((.not. associated (i_ptr, aa)) .or. (.not. associated (a_ptr, bb))) error stop 1_4

end

