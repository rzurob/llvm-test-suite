! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (diagnostic test: private parent
!*                               type results in private parent component)
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
    private
    type base
        integer :: id
    end type

    type, extends(base), public :: child
        character (20) :: name
    end type
end module

program fext029d2
use m
    type(child) :: c1

    c1%id = 10
    c1%base%id = 10     !<-- c1%base is illegal reference

    print *, c1%base
end
