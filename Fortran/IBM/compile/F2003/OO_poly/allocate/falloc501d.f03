! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (source-expr must be conformable to
!                               the allocate object in shape)
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

program falloc501d
    type base
        real*8 :: r1 = 1.0
        integer*2 :: i1 = 1
    end type

    type (base), allocatable :: b1(:), b3
    type (base) :: b2 (10)

    b2 = base (10.0, 10)

    allocate (b1(5), source=(/(b2(j),j=10,6)/))  !<-- this is illegal
end
