! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/21/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE is scalar
!*    FSOURCE is scalar
!*    MASK is scalar
!*    Non-poly
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base
        integer :: i = 8
    end type
end module

program merge001
use m
    type(Base) :: b1
    type(Base) :: b2

    print *, merge(Base(2), Base(-2), .TRUE.)
    print *, merge(Base(2), Base(-2), .FALSE.)
    if(.NOT. same_type_as(merge(Base(2), Base(-2), .FALSE.), Base(1))) &
     error stop 1_4

    b1%i = 3
    b2%i = 4

    print *, merge(b1, b2, .TRUE.)
    print *, merge(b1, b2, .FALSE.)
    if(.NOT. same_type_as(merge(b1, b2, .TRUE.), Base(1))) &
     error stop 2_4
end
