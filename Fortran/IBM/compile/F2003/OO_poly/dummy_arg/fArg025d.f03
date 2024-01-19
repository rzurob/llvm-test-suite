! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (C503; abstract type can
!                               not appear in declaration-type-spec-stmt with
!                               TYPE())
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
    type, abstract :: base
        integer*4 :: id
    end type

    procedure (type (base)) :: func1   !<-- illegal
end module

program fArg025d
end
