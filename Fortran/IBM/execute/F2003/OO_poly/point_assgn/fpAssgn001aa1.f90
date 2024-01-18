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
    type A
        integer i
    end type

    type B
        type (A) a1
    end type
end module

program fpAssgn001aa1
use m
    class(*), pointer :: i_ptr
    class(*), pointer :: a_ptr

    type (A), target :: aa
    type (B), target :: bb

    i_ptr => aa%i

    a_ptr => bb%a1

    if ((.not. associated (i_ptr, aa)) .or. (.not. associated (a_ptr, bb))) error stop 1_4

end

