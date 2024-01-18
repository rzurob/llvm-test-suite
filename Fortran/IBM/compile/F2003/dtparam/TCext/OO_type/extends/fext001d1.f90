! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (C424)
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

program fext001d1
    type A(k1)
        integer, kind :: k1
    end type

    type, extends(A):: DoublePrecision(k2)  !<-- this is illegal
        integer, kind :: k2
        real(k2) data
    end type

    type, extends(A) :: Real(k3)            !<-- this is illegal
        integer, kind :: k3
    end type

    type, extends(A) :: Character(n1)       !<-- this is illegal
        integer, len :: n1
        character(n1), pointer :: data
    end type

    type, extends(A) :: Integer(k4)         !<-- this is illegal
        integer, kind :: k4
        type(integer(k4)), pointer :: data
    end type

    type, extends(A) :: Complex(k5)         !<-- this is illegal
        integer, kind :: k5
        class(*), allocatable :: data
    end type

    type, extends(A) :: Logical(k6)         !<-- this is illegal
        integer, kind :: k6
        type(logical(k6)), pointer :: illegal
    end type

    type, extends(A) :: byte(k7)        !<-- this is legal
        integer, len :: k7
        byte legal
    end type
end
