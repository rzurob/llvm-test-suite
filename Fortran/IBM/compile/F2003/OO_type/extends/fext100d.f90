! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DERIVED TYPE (C614: 2nd part: in a structure
!                               component reference a part-name to the right of
!                               a part-ref with nonzero rank must not have
!                               allocatable or pointer attributes)
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

program fext100d
    type A
        integer, allocatable :: i
    end type

    type B
        class (A), pointer:: x
    end type

    type (A) :: a1(100)
    type (B) :: b1 (10)

    print *, a1%i, b1%x%i
end
