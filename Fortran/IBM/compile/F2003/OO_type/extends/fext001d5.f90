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
    type base
    end type

    type, extends(base) :: child
        sequence                !<-- illegal
        integer(4) :: i
    end type

    type seq1
        sequence
        integer(2) i1
    end type

    type, extends(seq1) :: seq2     !<-- illegal
        sequence                    !<-- we have an extra error message here

        integer(4) i2
    end type
end
