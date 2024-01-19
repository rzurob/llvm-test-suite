! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
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

program falloc003d2
    type base
    end type

    type, extends(base) :: child
        integer*4 id
    end type

    type (base), pointer :: b1

    class (child), allocatable :: c1

    allocate (b1, source=child(10))   !<-- illegal

    allocate (c1, source=base())      !<-- illegal
end
