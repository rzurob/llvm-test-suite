! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 277245; problem #3:
!                               structure constructor in an implied-do in array
!                               constructor for structure with allocatable
!                               components)
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
        integer(4) :: id
        integer(4), allocatable :: data
    end type

    type base1
        integer(4), allocatable :: data(:)
    end type
end module

program fmisc010b
use m
    type (base) :: b1(2:11)

    type (base1) :: b11 (3)

    b1 = (/(base(i, i), i = 1, 10)/)

    if (any (b1%id /= (/1,2,3,4,5,6,7,8,9,10/))) error stop 1_4

    do i = 1, 10
        if (b1(i+1)%data /= i) error stop 2_4
    end do

    b11 = (/(base1((/(i, i=1,j)/)), j=1,3)/)

    if (any (b11(1)%data /= (/1/))) error stop 3_4
    if (any (b11(2)%data /= (/1, 2/))) error stop 4_4
    if (any (b11(3)%data /= (/1, 2, 3/))) error stop 5_4
end

