! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-08-29
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : For explicit shape coarray dummy and actual,
!                               there is no requirement that the corank to
!                               match; test complex(double); also test that
!                               a coarray subobject (that doesn't make copies)
!                               are also a coarray.
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

module cmplx_test
implicit none
    contains

    !this routine only compute multiplication of two coarrays diagonally
    ! given the co-shape of the inputs are [2,*], only two elements in the input
    ! data are used in the computation: [0,0] & [1, 1]
    subroutine cmplx_multiple_diag (a, b, c, n)
        integer, intent(in) :: n
        complex(8), intent(in) :: a(n)[0:1,0:*], b(n)[0:1,0:*]
        complex(8), intent(out) :: c(n+1)[0:*]

        integer :: me, np

        np = num_images()

        ! we need a minimum of 4 images
        if (np < 4) then
            print *, 'The program needs at least four images'
            stop 'condition unsatified'
        end if

        me = this_image()

        if ((me == image_index(a,[0,0])) .or. &
            (me == image_index(a,[1,1]))) then
            c(:n) = a(:) * b(:)

            if (me == image_index(a,[0,0])) then
                c(n+1) = a(1) * b(1)[1,1]
            else
                c(n+1) = a(1) * b(1)[0,0]
            end if
        else
            c(:) = cmplx(0.5d0,0.5d0,8)
        end if

        sync all
    end subroutine
end module

program test_complex
use cmplx_test
    implicit none
    integer, parameter :: n = 100

    complex(8), save :: x(2*n)[*], y(2*n)[*], z(2*n+2)[*]

    call set_cmplx_val

    call cmplx_multiple_diag (x, y, z,n)

    call cmplx_multiple_diag (x(n+1), y(n+1:), z(n+2),n)

    call verifyZ(z,n)

    contains

    subroutine set_cmplx_val
        integer i, me

        me = this_image()

        do i = 1, 2*n
            x(i) = cmplx(i, i*me, 8)
            y(i) = cmplx(me, i + me, 8)
        end do
        sync all
    end subroutine

    subroutine verifyZ(x, n)
        integer, intent(in) :: n
        complex(8), intent(in) :: x(2*n+2)[0:1,0:*]

        logical, external :: precision_x6
        integer :: me, i

        me = this_image()

        if ((me == 1) .or. (me == 4)) then
            do i = 1, n
                if (.not. precision_x6(x(i), cmplx(i, i*me, 8)*cmplx(me, i+me, 8))) then
                    print *, 'failed to verify on image,', me, ': element', i
                    print *, x(i), 'vs', cmplx(i, i*me, 8)*cmplx(me, i+me, 8)
                    error stop 1
                end if
            end do

            do i = n+2, 2*n+1
                if (.not. precision_x6(x(i), cmplx(i-1, (i-1)*me, 8)*cmplx(me, i-1+me, 8))) then
                    print *, 'failed to verify on image', me, ': element', i
                    print *, x(i), 'vs', cmplx(i-1, (i-1)*me, 8)*cmplx(me, i-1+me, 8)
                    error stop 1
                end if
            end do

            ! test values of x(n+1) and x(2*n+2) are special
            if (me == 1) then
                if ((.not. precision_x6(x(n+1), cmplx(1,1,8)*cmplx(4,5,8))) .or.&
                    (.not. precision_x6(x(2*n+2), cmplx(n+1,n+1,8)*cmplx(4,n+5,8)))) then
                    print *, 'failed to verify on image 1: element', n+1, 'or', 2*n+2
                    print *, x(n+1), 'vs', cmplx(1,1,8)*cmplx(4,5,8)
                    print *, x(2*n+2), 'vs', cmplx(n+1,n+1,8)*cmplx(4,n+5,8)
                    error stop 1
                end if
            else if (me == 4) then
                if ((.not. precision_x6(x(n+1), cmplx(1,4,8)*cmplx(1,2,8))) .or.&
                    (.not. precision_x6(x(2*n+2), cmplx(n+1,(n+1)*4,8)*cmplx(1,n+2,8)))) then
                    print *, 'failed to verify on image 1: element', n+1, 'or', 2*n+2
                    print *, x(n+1), 'vs', cmplx(1,4,8)*cmplx(1,2,8)
                    print *, x(2*n+2), 'vs', cmplx(n+1,(n+1)*4,8)*cmplx(1,n+2,8)
                    error stop 1
                end if
            end if
        else
            do i = 1, size(x)
                if (.not. precision_x6(x(i), cmplx(0.5d0, .5d0, 8))) then
                    print *, 'verification failed on image', me
                    print *, x(i), 'vs', cmplx(0.5d0, .5d0, 8)
                    error stop 1
                end if
            end do
        end if
    end subroutine
end
