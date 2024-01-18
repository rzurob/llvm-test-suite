! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr051a1.f
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        double precision :: value(2)
    end type

    type, extends(base) :: child(k2)    ! (4,20,4)
        integer, kind :: k2
        integer(k2)   :: i
    end type

    type container(k3,n2)    ! (4,20)
        integer, kind     :: k3
        integer, len      :: n2
        type(base(k3,n2)) :: data1 (5)
        type(base(k3,n2)) :: data2 (2)
    end type
end module

program fconstr051a1
use m
    class (base(4,20)), allocatable :: b1(:)
    class (base(4,20)), pointer :: b2(:,:)

    type (child(4,20,4)), target :: c1(0:1,2)

    b2 => c1

    c1%i = reshape ((/1,2,3,4/), (/2,2/))

    do j = 1, 2
        do i = 0, 1
            c1(i,j)%value = (/i, j/)
        end do
    end do

    allocate (b1(10), source=(/(child(4,20,4)((/i, i+1/), i), i=1,10)/))

    !! test the structure constructor for container type

!    associate (x1 => container(4,20)(data1=b1(::2), data2=b2(:,2)))
    call associate_replacer (x1 = container(4,20)(data1=b1(::2), data2=b2(:,2)))

    contains

!    associate (x1 => container(4,20)(data1=b1(::2), data2=b2(:,2)))
    subroutine associate_replacer(x1)
        type(container(4,*)), intent(in) :: x1

        write (*, '(10f10.2)') x1%data1
        write (*, '(4f10.2)') x1%data2
    end subroutine
end
