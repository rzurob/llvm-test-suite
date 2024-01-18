! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-01-07
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : test assignment on coindexed objects (LHS) of
!                               complex type: test scalar and rank1 array.
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

module data_mod
    implicit none
    complex, save :: cx1[*]
    complex(8), save :: dcx1(100)[*], dcx2(200)[*]
end module

program assign_cmplx1
    use data_mod
    implicit none

    complex, save :: cx2[*]
    real(8), save :: d1(100)[*], dd(100)
    real, save :: r1, r2[*]

    integer :: me, np, left, right, i

    np = num_images()
    me = this_image()

    if (np < 2) then
        print *, 'Error: program requires at least two images to run'
        error stop 1
    end if

    ! find out left neighbor's index
    if (me == 1) then
        left = np
    else
        left = me - 1
    end if

    ! find out the right neighbor's index
    if (me == np) then
        right = 1
    else
        right = me + 1
    end if

    r1 = 1.5*right
    dd = [(i, i = 1, size(dd))]*1.0d0

    ! assign the coarrays of floating point with neigboring images
    r2[right] = r1 - 0.5*right

    d1(::2)[left] = dd(::2)*left
    d1(2::2)[right] = dd(2::2)*right
    sync all

    call verify_real_val

    ! next assign complex data
    cx1[right] = cmplx(d1(1)[right],0.0)
    cx2[left] = cmplx(0.0, d1(2)[left])

    dcx1(1:50)[left] = cmplx(d1(1:50)[left], 1.0d0, 8)
    dcx1(51:)[right] = cmplx(d1(51:)[right], 1.0d0, 8)
    sync all

    dcx2(::2)[right] = dcx1(:)[right]
    dcx2(2::2)[left] = cmplx(1.0d0, dd(:)*left, 8)

    sync all

    call verify_cmplx_val

    contains

    ! this routine verifies the results of coarrays of real data type
    subroutine verify_real_val
        logical, external :: precision_r4, precision_r8
        integer i

        if (.not. precision_r4(r2, 1.0*me)) then
            print *, 'Error: test r2 failed on image',me
            print *, 'Expected:',1.0*me, 'Actual:',r2
            error stop 1
        end if

        do i = lbound(d1,1), ubound(d1,1)
            if (.not. precision_r8(d1(i), i*1.0d0*me)) then
                print *, 'Error: test d1 failed on image',me
                print *, 'element:',i,'Expected:',i*1.0d0*me, 'Actual:',d1(i)
                error stop 1
            end if
        end do
    end subroutine

    ! this routine verifies the results of coarrays of complex data type
    subroutine verify_cmplx_val
        logical, external :: precision_x8, precision_x6
        integer i

        ! verify cx1
        if (.not. precision_x8(cx1, cmplx(d1(1), 0.0, 4))) then
            print *, 'Error: test cx1 failed on image',me
            print *, 'Expected:',cmplx(d1(1), 0.0), 'Actual:',cx1
            error stop 1
        end if

        ! verify cx2
        if (.not. precision_x8(cx2, cmplx(0.0, d1(2),4))) then
            print *, 'Error: test cx2 failed on image',me
            print *, 'Expected:',cmplx(0.0, d1(2),4), 'Actual:',cx2
            error stop 1
        end if

        ! verify dcx1
        do i = 1, 100
            if (.not. precision_x6(dcx1(i), cmplx(d1(i), 1.0d0, 8))) then
                print *, 'Error: test dcx1 failed on image',me
                print *, 'element:',i,'Expected:',cmplx(d1(i), 1.0d0, 8), 'Actual:',dcx1(i)
                error stop 1
            end if
        end do

        ! verify dcx2
        do i = 1, 200, 2
            if (.not. precision_x6(dcx2(i), dcx1((i+1)/2))) then
                print *, 'Error: test dcx2 failed on image',me
                print *, 'element:',i,'Expected:',dcx1((i+1)/2), 'Actual:',dcx2(i)
                error stop 1
            end if
        end do

        do i = 2, 200, 2
            if (.not. precision_x6(dcx2(i), cmplx(1.0d0,dd((i+1)/2)*me,8))) then
                print *, 'Error: test dcx2 failed on image',me
                print *, 'element:',i,'Expected:',cmplx(1.0d0,dd((i+1)/2)*me,8), 'Actual:',dcx2(i)
                error stop 1
            end if
        end do
    end subroutine
end
