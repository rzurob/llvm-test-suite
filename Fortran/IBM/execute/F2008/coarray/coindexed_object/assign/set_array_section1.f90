! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-01-14
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : test assignment of coindexed objects in
!                               intrinsic assignments used as LHS.  The data
!                               type is real(8) and integer(8), and use the
!                               array section.
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
    real(8), save :: d1(10,10)[*]
    integer(8), save :: i1(100)[*] = -1
end module

module op_mod
    implicit none
    contains

    subroutine set_val_r8 (d, val, img)
        real(8), intent(inout) :: d(10,10)[*]
        real(8), intent(in) :: val (:,:)
        integer, intent(in) :: img

        if (any (shape(val) /= 5)) then
            print *, 'Error 1: input val"shpe is incorrect'
            error stop 1
        end if

        ! do the assignment using img index
        d(::2,::2)[img] = val
        sync all
    end subroutine

    subroutine set_val_i8 (ival_right)
        use data_mod, only : i1
        integer(8), intent(in) :: ival_right(:)

        integer :: np, me, right

        np = num_images()
        me = this_image()

        if (me == np) then
            right = 1
        else
            right = me + 1
        end if

        i1(::10)[right] = ival_right(1:10)
        sync all
    end subroutine
end module

program set_array_section1
    use data_mod, only : d1
    use op_mod, only : set_val_r8, set_val_i8
    implicit none

    integer :: np, me, left, i, j, right
    real(8), save :: d2(10,10)[*]
    integer(8) i2(10)

    np = num_images()
    me = this_image()

    if (np < 2) then
        print *, 'Error: requires at least 2 images to run the program.'
        error stop 1
    end if

    if (me == 1) then
        left = np
    else
        left = me - 1
    end if

    if (me == np) then
        right = 1
    else
        right = me + 1
    end if

    d1(:,:) = -1.0
    d2 = reshape ([(i, i = 1, 100)]*me, [10,10])
    i2 = [(i, i = 1, 10)]*right

    sync all

    call set_val_r8 (d1, d2(::2,::2)[left], left)

    call set_val_i8 (i2)

    call verify_d1d2

    call verify_i1

    contains

    subroutine verify_i1
        use data_mod, only: i1
        integer i

        do i = 1, 100, 10
            if (i1(i) /= (i/10+1)*me) then
                print *, 'Error: failed to verify i1 on image', me
                print *, 'element ', i, ', Expected:', i/10+1, ', Actual:', i1(i)
                error stop 1
            end if

            if (any(i1(i+1:i+9) /= -1)) then
                print *, 'Error: failed to verify i1 on image', me
                print *, 'elements from', i+1, 'to',i+9,', Expected: ', -1, &
                         ', Actual:', i1(i+1:i+9)
                error stop 1
            end if
        end do
    end subroutine

    subroutine verify_d1d2
        logical, external :: precision_r8
        integer i, j, k

        k = 1
        do j = 1, 10
            do i = 1, 10
                if (.not. precision_r8 (d2(i,j), k*1.0d0*me)) then
                    print *, 'Error: failed to verify d2 on image', me
                    print *, 'Element (',i,',',j,'), Expected: ', k*1.0d0*me, &
                             'Actual: ',d2(i,j)
                    error stop 1
                end if

                if ((mod(i,2) == 0) .or. (mod(j,2) == 0)) then
                    if (.not. precision_r8 (d1(i,j), -1.0d0)) then
                        print *, 'Error: failed to verify d1 on image', me
                        print *, 'Element (',i,',',j,'), Expected: ', -1.0d0, &
                             'Actual: ',d1(i,j)
                        error stop 1
                    end if
                else
                    if (.not. precision_r8 (d1(i,j), d2(i,j))) then
                        print *, 'Error: failed to verify d1 on image', me
                        print *, 'Element (',i,',',j,'), Expected: ', d2(i,j), &
                             'Actual: ',d1(i,j)
                        error stop 1
                    end if
                end if
                k = k + 1
            end do
        end do
    end subroutine
end
