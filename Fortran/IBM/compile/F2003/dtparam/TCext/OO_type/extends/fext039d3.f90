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
    type p(k1)
        integer, kind :: k1
        sequence
        real(k1) :: value = 1.0
    end type

    type, bind (c) :: q
        real*4 :: value = 1.0
    end type

    type base(k3)
        integer, kind :: k3
        integer(k3) :: id
        class (p(k3)), pointer :: value => null()   !<-- illegal
    end type

    type base1(k4)
        integer, kind :: k4
        class (q), pointer :: value => null()   !<-- illegal
    end type
end module

program fext039d3

end

