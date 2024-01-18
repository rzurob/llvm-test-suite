! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (C429: sequence and extends specified
!                               for the same type)
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

program fext001d5
    type base(k1)
        integer, kind :: k1
    end type

    type, extends(base) :: child(k2)
        integer, kind :: k2
        sequence                !<-- illegal
        integer(k2) :: i
    end type

    type seq1(k3)
        integer, kind :: k3
        sequence
        integer(k3) i1
    end type

    type, extends(seq1) :: seq2(k4)   !<-- illegal
        integer, kind :: k4
        sequence                      !<-- we have an extra error message here

        integer(k4) i2
    end type
end
