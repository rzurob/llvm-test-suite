! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (use of the array section
!                               from poly-arrays as the data-source for the
!                               nonpoly non-allocatable/pointer components)
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
        double precision :: value(2)
    end type

    type, extends(base) :: child
        integer(4) :: i
    end type

    type container
        type (base) :: data1 (5)
        type (base) :: data2 (2)
    end type
end module

program fconstr051a1
use m
    class (base), allocatable :: b1(:)
    class (base), pointer :: b2(:,:)

    type (child), target :: c1(0:1,2)

    b2 => c1

    c1%i = reshape ((/1,2,3,4/), (/2,2/))

    do j = 1, 2
        do i = 0, 1
            c1(i,j)%value = (/i, j/)
        end do
    end do

    allocate (b1(10), source=(/(child((/i, i+1/), i), i=1,10)/))

    !! test the structure constructor for container type
    associate (x1 => container(data1=b1(::2), data2=b2(:,2)))
        write (*, '(10f10.2)') x1%data1
        write (*, '(4f10.2)') x1%data2
    end associate
end
