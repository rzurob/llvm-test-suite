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
    type A
        integer :: a1 = 10
    end type

    type B
        integer(8) :: b1 = -1
    end type

    type, extends(A), extends(B) :: c   !<-- error message issued here; B wins
    end type

    type (c) :: c1

    print *, c1%a1                      !<-- illegal reference
    print *, c1%b1
end