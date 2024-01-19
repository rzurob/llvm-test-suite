! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ABSTRACT type (C428: if ABSTRACT appears, then
!                               the type must be extensible)
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
        sequence
        integer(k1) :: id
    end type

    type, abstract, bind(c) :: base1
        integer*4 :: id
    end type
end module

program fabst001d
end
