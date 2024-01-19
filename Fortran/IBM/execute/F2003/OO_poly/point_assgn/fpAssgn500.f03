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
    type base
    end type

    type, extends (base) :: child
        integer(8) :: something
    end type

    class (*), pointer :: x, x1(:)

    type (child), target :: c1, c2(10)

    x => c1%base
    x1 => c2%base

    if (associated (x, c1%base) .or. associated (x1, c2%base)) error stop 1_4
    if (associated (x, c1) .or.  associated (x1, c2)) error stop 2_4
end

