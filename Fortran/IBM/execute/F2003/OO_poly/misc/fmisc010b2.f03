! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 277245; problem #3
!                               structure constructor with allocatable component
!                               in implied do in array constructor)
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
        integer(8), allocatable :: data (:,:)
    end type
end module

program fmisc010b2
use m
    integer(8)   a1(10,10,10), i

    type (base) :: b1(10)

    a1 = reshape ((/(i, i=1_8,1000_8)/), (/10,10,10/))

    b1 = (/(base(a1(:,:,j)), j=1,10)/)

    !! verify a1 and b1
    do j1 = 1, 10
        do j2 = 1, 10
            do j3 = 1, 10
                if (a1(j3,j2,j1) /= (j1-1)*100 + (j2 - 1)*10 + j3) error stop 1_4
            end do
        end do
    end do

    do j = 1, 10
        if (any (b1(j)%data /= a1(1:10,1:10,j))) error stop 2_4
    end do
end
