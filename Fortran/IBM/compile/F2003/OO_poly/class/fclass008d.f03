! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (poly-entities can be in the
!                                array constructor; but they shall have the same
!                                declared types)
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

program fclass008d
use m
    class (base), allocatable :: b1, b2(:)

    class (child), allocatable :: c1

    allocate (child:: b1, b2(2))

    allocate (c1)

    print *, shape (reshape ((/b1, b2, c1/), (/2,2/)))
end