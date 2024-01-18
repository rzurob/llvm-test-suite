! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ABSTRACT (C611, if appearing at the right-most
!                               part in a data-ref, it must not be polymorphic
!                               for abstract type)
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
    type, abstract :: base(k1)
        integer, kind :: k1
        integer(k1) :: id
    end type

    type, extends(base) :: child(k2,n)
        integer, kind :: k2
        integer, len :: n
    end type
end module

use m
    type (child(4,4,20)), target :: c1

    class (base(4)), pointer :: b

    print *, c1%base%id     !<-- this is legal
    b => c1%base            !<-- this is illegal
end
