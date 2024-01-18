! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-19
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 381172
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
module setArrayMod
    implicit none
    integer, parameter :: double = selected_real_kind(12)

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
    use setArrayMod
    implicit none

    integer, parameter :: localSize = 10
    real(double), save :: x(localSize)[*]
    real(double), allocatable :: allX(:)
    logical, external :: precision_r8

    integer :: me, np, i, j, allocateSize, k

    me = this_image()
    np = num_images()

    allocateSize = localSize
    if (me == np) then
        allX = dlog ([(i*1.5d0, i = 1, np*allocateSize)])
        call distributeArray (x, localSize, allX)

        deallocate (allX)
    end if
    sync all

    !! verify the results
    if (me == 1) then
        k = 1
        do i = 1, np
            do j = 1, localSize
                if (.not. precision_r8(dlog(k*1.5d0), x(j)[i])) then
                    print *, 'validation for element',j,'on image',i,'failed'
                    print *, dlog(k*1.5d0), 'vs', x(j)[i]
                    error stop 1
                end if
                k = k + 1
            end do
        end do
    end if
end

