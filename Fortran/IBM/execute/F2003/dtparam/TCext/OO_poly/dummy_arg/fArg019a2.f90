! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg019a2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (arrays used in the
!                               defined assignment)
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
    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: data (:) => null()

        contains

        procedure :: print => printBase
    end type

    interface assignment (=)
        subroutine assgnArrayFromArray (b1, b2)
        import base
            class (base(4)), intent(inout) :: b1 (:)
            class (base(4)), intent(in) :: b2(:)
        end subroutine

        subroutine assgnArrayFromScalar (b1, b2)
        import base
            class (base(4)), intent(inout) :: b1 (:)
            class (base(4)), intent(in) :: b2
        end subroutine
    end interface

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        if (.not. associated (b%data)) then
            print *, 'null pointer'
        else
            print *, 'bounds:', lbound(b%data), ubound(b%data)
            print *, b%data
        end if
    end subroutine
end module

program fArg019a2
use m
    type (base(4)) :: b1(10), b2

    class (base(4)), pointer :: b_ptr (:)

    allocate (b_ptr (10))

    do i = 1, 5
        allocate (b_ptr(i)%data(i:2*i), source=(/(j,j=i,2*i)/))
    end do

    b_ptr(6:) = b_ptr(:5)

    do i = 1, 10
        call b_ptr(i)%print
    end do

    allocate (b2%data(-1:1), source=(/-1,0,1/))


    !! assignment to array from scalar
    b1 = b2

    do i = 1, 10
        call b1(i)%print
    end do
end


subroutine assgnArrayFromArray (b1, b2)
use m, only:base
    class (base(4)), intent(inout) :: b1 (:)
    class (base(4)), intent(in) :: b2(:)

    do i = 1, size (b1)
        if (associated (b1(i)%data)) deallocate (b1(i)%data)

        if (associated (b2(i)%data)) then
            allocate (b1(i)%data(lbound(b2(i)%data,1):ubound(b2(i)%data,1)))

            b1(i)%data = b2(i)%data
        end if
    end do
end subroutine


subroutine assgnArrayFromScalar (b1, b2)
use m, only:base
    class (base(4)), intent(inout) :: b1 (:)
    class (base(4)), intent(in) :: b2

    do i = 1, size (b1)
        if (associated (b1(i)%data)) deallocate (b1(i)%data)

        if (associated (b2%data)) then
            allocate (b1(i)%data(size(b2%data)))
            b1(i)%data = b2%data
        end if

    end do
end subroutine
