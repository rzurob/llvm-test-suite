! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extension (base type and extended
!*                               type both have no components)
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
    type base
    end type

    type, extends(base) :: child
    end type

    type (base) :: b1_m
    type (child) :: c1_m
end module

program fext016
    use m

    type, extends(base) :: secondChild
    end type

    type (child) :: c1
    type (base) :: b1
    type (secondChild) :: s1

    print *, b1, c1
    print *, b1_m, c1_m
    print *, s1

end
