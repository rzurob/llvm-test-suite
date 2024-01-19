! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (variables with PROTECTED
!                               attribute shall not be used in the structure
!                               constructor as a data target for pointer
!                               component)
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
    integer*4, protected, target :: i1
end module

program fconstr046d
use m

    type base
        integer*4, pointer :: i1
    end type

    type container
        class (*), pointer :: x
    end type

    type (base) :: b1
    type (container) :: co

    co = container (i1)     !<-- this is illegal

    b1 = base (i1)      !<-- this is illegal
end
