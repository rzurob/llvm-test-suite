! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (component of a sequence type must be
!                               declared as intrinsic types or of sequence
!                               derived type)
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

program fext039d4
    type base(k1)
        integer, kind :: k1
        integer(k1) :: i1
    end type

    type seq1(k2)
        integer, kind :: k2
        sequence
        integer(k2), pointer :: id => null()
        class (base(k2)), pointer :: data => null()     !<-- illegal
        type (seq1(4)), pointer :: next => null()
    end type
end