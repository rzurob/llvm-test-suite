! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-07
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : test explicit-shape coarray dummy.  Type is
!                               real type.
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

module computerOne1Image
    implicit none
    integer, parameter :: double = selected_real_kind(12)

    contains

    function compute(coarr, m, n, which_img, idx, f) result(res)
        integer, intent(in) :: m,n, which_img, idx(2)
        real(double), intent(inout), dimension(m,n) :: coarr[*]
        real(double) res
        interface
            real(double) function f(x)
            import :: double
                real(double), intent(in) :: x
            end function
        end interface

        if (any(idx > [m,n])) error stop 3

        res = f(coarr(idx(1), idx(2))[which_img])
    end function
end module

module setArrayMod
    use computerOne1Image, only: double
    implicit none

    contains

    subroutine distributeArray (coarr, m, arr)
        integer, intent(in) :: m
        real(double), intent(out) :: coarr(m)[*]
        real(double), intent(in) :: arr(*)

        integer me, np, i
        me = this_image()
        np = num_images()

        ! this routine should be run from 1 image
        if (me == np) then
            do i = 1, np
                coarr(:)[i] = arr((i-1)*m+1:i*m)
            end do
        end if
    end subroutine
end module

program compute1Image
    use computerOne1Image, only: double, compute
    use setArrayMod, only:distributeArray
    implicit none

    integer, parameter :: localSize = 100
    real(double), save :: x(localSize)[*]
    real(double), allocatable :: allX(:)
    logical, external :: precision_r8
    intrinsic dsin

    integer :: me, np, i, allocateSize

    me = this_image()
    np = num_images()

    allocateSize = localSize / 2
    if (me == np) then
        allX = dlog ([(i*1.5d0, i = 1, np*allocateSize)])
        call distributeArray (x, allocateSize, allX)
    end if

    sync all

    ! now let's verify the results
    call verify_val

    sync all

    contains

    subroutine verify_val
        integer i,j, k, img
        real(double) tmp

        if (me == 1) then
            k = 1
            do img = 1, np
                do j = 1, allocateSize/5
                    do i = 1, 5
                        tmp = compute(x,5,allocateSize/5,img,[i,j], dsin)

                        if (.not. precision_r8(tmp, dsin(dlog(1.5d0*k)))) then
                            print *, 'verification failed on image', img
                            print *, tmp, 'vs', dsin(dlog(1.5d0*k))
                            error stop 1
                        end if

                        k = k + 1
                    end do
                end do
            end do
        end if
    end subroutine
end
