! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/point_assgn/fpAssgn500.f
! opt variations: -qnok -qnol

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn500.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (special cases where
!                               zero-size storage unit for pointer target in
!                               ASSOCIATED())
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

program fpAssgn500
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends (base) :: child(n2,k2)    ! (4,20,20,8)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)   :: something
    end type

    class (*), pointer :: x, x1(:)

    type (child(4,20,20,8)), target :: c1, c2(10)

    x => c1%base
    x1 => c2%base

    if (associated (x, c1%base) .or. associated (x1, c2%base)) error stop 1_4
    if (associated (x, c1) .or.  associated (x1, c2)) error stop 2_4
end

