! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived type (an empty type; allocatable and
!*                               pointer array in allocate statement)
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

program fext100
    type base
    end type

    class (base), allocatable :: b1(:)
    class (base), pointer :: b2(:)

    allocate (b1(10), source=base())
    allocate (b2(20), source=base())

    deallocate (b2, b1)
end
