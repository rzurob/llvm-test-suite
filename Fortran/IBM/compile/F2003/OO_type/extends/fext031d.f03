! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 10, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (extends and private on the same
!*                               type definition)
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

    type, extends(base), private :: child
        character(20) :: name
    end type

    type, extends(child) :: thirdGeneration
        logical*2 :: isSet
    end type

    type (thirdGeneration) :: t1_m
    type (child) :: c1_m

end module

program fext031d
    use m

    type (thirdGeneration) :: t1

    t1_m%child%id = 1   !<-- illegal
    t1%child%id = 2     !<-- illegal

    t1%child = c1_m     !<-- illegal
    t1_m%child = c1_m   !<-- illegal
end