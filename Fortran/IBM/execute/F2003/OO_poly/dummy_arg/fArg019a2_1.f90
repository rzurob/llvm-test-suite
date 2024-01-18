!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg019a2_1.f
! %VERIFY: fArg019a2_1.out:fArg019a2_1.vf
! %STDIN:
! %STDOUT: fArg019a2_1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (arrays used in defined
!                               assignment)
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
        integer*4, pointer :: data (:) => null()
    end type

    interface assignment (=)
        subroutine assgnArrayFromArray (b1, b2)
        import base
            class (base), intent(inout) :: b1 (:)
            class (base), intent(in) :: b2(:)
        end subroutine

        subroutine assgnArrayFromScalar (b1, b2)
        import base
            class (base), intent(inout) :: b1 (:)
            class (base), intent(in) :: b2
        end subroutine
    end interface
end module

program fArg019a2_1
use m
    type (base) :: b1(10), b2

    class (base), pointer :: b_ptr (:)

    allocate (b_ptr (10))

    do i = 1, 5
        allocate (b_ptr(i)%data(i))
        b_ptr(i)%data = (/(j,j=1,i)/)
    end do

    b_ptr(6:) = b_ptr(:5)

    b1 = b_ptr

    do i = 1, 10
        print *, b1(i)%data
        deallocate (b_ptr(i)%data)
    end do

    allocate (b2%data (3), source=(/101,201,301/))


    b1 = b2

    do i = 1, 10
        print *, b1(i)%data
        deallocate (b1(i)%data)
    end do

    deallocate (b_ptr)
end


subroutine assgnArrayFromArray (b1, b2)
use m, only:base
    class (base), intent(inout) :: b1 (:)
    class (base), intent(in) :: b2(:)

    do i = 1, size (b1)
        if (associated (b1(i)%data)) deallocate (b1(i)%data)

        if (associated (b2(i)%data)) then
            allocate (b1(i)%data(size(b2(i)%data)))
            b1(i)%data = b2(i)%data
        end if
    end do
end subroutine


subroutine assgnArrayFromScalar (b1, b2)
use m, only:base
    class (base), intent(inout) :: b1 (:)
    class (base), intent(in) :: b2

    do i = 1, size (b1)
        if (associated (b1(i)%data)) deallocate (b1(i)%data)

        if (associated (b2%data)) then
            allocate (b1(i)%data(size(b2%data)))
            b1(i)%data = b2%data
        end if

    end do
end subroutine
