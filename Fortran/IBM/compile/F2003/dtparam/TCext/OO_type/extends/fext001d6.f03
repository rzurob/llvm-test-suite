! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : extends (C1503, extends keyword can not be used
!                               for a bind(c) type)
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

program fext001d6
use iso_c_binding

    type base(k1)
        integer, kind :: k1
    end type

    type, extends(base), bind(C) :: child  !<-- illegal
        integer(c_short) :: s1
    end type

    type, bind(c) :: bType1
        integer(c_short) :: s1
    end type

    type, bind(c), extends(bType1) :: bType2    !<-- illegal
    end type
end
