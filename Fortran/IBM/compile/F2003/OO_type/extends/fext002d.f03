! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extended (component inherited,
!                                private components)
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
        integer, private :: id = 10
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

end module

program fext002d
    use m
    type, extends(base) :: secondChild
        character(20) :: name
    end type

    type (secondChild) :: c2

    c2%name = 'c2'

    print *, c2     !<-- illegal as c2 has inaccessible component

end
