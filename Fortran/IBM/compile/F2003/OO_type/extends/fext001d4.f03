! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (C426, part2 parent type must be
!                               extensible)
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

program fext001d4
use iso_c_binding

    type seq1
        sequence

        integer i1, i2
    end type

    type, bind(c) :: bType
        integer(c_short) :: s1
    end type

    type, extends(seq1) :: bad1     !<-- illegal
    end type

    type, extends (bType) :: bad2   !<-- illegal
    end type
end
