!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 09/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use of the procedure pointer for the expr;
!                               involves the intrinsic types only.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    abstract interface
        real function genRealArray (r)
            real, intent(in) :: r(:)

            allocatable genRealArray(:)
        end function
    end interface

    contains

    subroutine tt (array, f, r)
        real, allocatable :: array(:)
        procedure(genRealArray), pointer :: f
        real, intent(in) :: r(:)

        if (associated(f)) array = f(r)
    end subroutine
end module

program procPtr001
use m
procedure(genRealArray) reverseOrder, doubleVal
    procedure(genRealArray), pointer :: p1

    real, allocatable :: r1(:)

    logical(4), external :: precision_r4

    p1 => reverseOrder

    call tt (r1, p1, (/(i*1.0, i=1,100)/))

    if ((.not. allocated(r1)) .or. (size(r1) /= 100)) error stop 1_4

    do i = 1, 100
        if (.not. precision_r4((101-i)*1.0_4, r1(i))) error stop 2_4
    end do

    p1 => doubleVal

    call tt (r1, p1, r1/4.0)

    do i = 1, 100
        if (.not. precision_r4((101-i)*5.e-1, r1(i))) error stop 3_4
    end do

    deallocate (r1)

    call tt (r1, null(), r1)

    if (allocated(r1)) error stop 4_4

end


real function reverseOrder (r)
    real, intent(in) :: r(:)

    allocatable reverseOrder(:)

    reverseOrder = r(size(r):1:-1)
end function


function doubleVal (r)
    real, intent(in) :: r(:)

    real, allocatable :: doubleVal(:)

    doubleVal = 2.0*r
end function
