! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : array constructor (all elements in the array
!                               constructor shall have the same declared type)
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
    type base
        integer*4 :: id
    end type

    type, extends (base) :: child
        character*20 :: name
    end type
end module

program fArg521d
use m
    class (base), allocatable :: b1(:)
    type (child) :: c1 (4), c2(1)

    c2 = child(2, 'temp2')

    allocate (b1(3), source=child(1,'temp'))

    c1 = (/b1, c2/)
end
