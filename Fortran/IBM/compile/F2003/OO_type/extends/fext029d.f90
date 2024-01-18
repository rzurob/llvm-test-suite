! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (private parent component if parent
!                               type is private)
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
    type, private :: base
        integer :: id
        real*4, private :: value
    end type

    type, extends(base) :: child
        character(20) :: name
    end type
end module


program fext029d
    use m

    type (child) :: c1

    c1%id = 10
    c1%base%id = 1          !<-- illegal reference using c1%base

    print *, c1%base        !<-- illegal reference using c1%base

end
