! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived type (class keyword for component, must
!*                               be extensible types)
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
    type p
        sequence
        real*4 :: value = 1.0
    end type

    type, bind (c) :: q
        real*4 :: value = 1.0
    end type

    type base
        integer*4 :: id
        class (p), pointer :: value => null()   !<-- illegal
    end type

    type base1
        class (q), pointer :: value => null()   !<-- illegal
    end type
end module

program fext039d3

end

