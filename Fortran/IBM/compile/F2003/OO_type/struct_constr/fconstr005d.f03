! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (can NOT appear before
!*                               the type is defined)
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
        integer*4 :: id
    end type

    type (child) :: c1_m = child ('c1_m')
    type (child) :: c2_m = child (name = 'c1_m')

    type, extends(base) :: child
        character(20) :: name
    end type
end module

program fconstr005d

end
