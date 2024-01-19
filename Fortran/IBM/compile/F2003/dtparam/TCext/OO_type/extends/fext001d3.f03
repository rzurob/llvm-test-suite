! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (C425, same EXTENDS keywords can not
!                               appear more than once in a given
!                               derived-type-stmt)
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

program fext001d3
    type A(k1)
        integer, kind :: k1
        integer(k1) :: a1 = 10
    end type

    type B(k2)
        integer, kind :: k2
        integer(k2) :: b1 = -1
    end type

    type, extends(A), extends(B) :: c(k3) !<-- error message issued here; B wins
        integer, kind :: k3
    end type

    type (c(8,4)) :: c1

    print *, c1%a1                      !<-- illegal reference
    print *, c1%b1
end
