! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/22/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Write data in POINT decimal mode and modified to
!                               be conform to COMMA mode.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    real(8), allocatable :: d1(:), d2(:,:,:,:)
end module


subroutine setRandomValues (d, i)
    double precision, intent(out) :: d(*)
    integer, intent(in) :: i

    call random_number (d(1:i))
end subroutine


program decEditDesc008a
use m
    integer, external :: newUnit

    logical(4), external :: precision_r8


    allocate (d2(10,10,10,10))

    allocate (d1(size(d2)))

    call setRandomValues (d2, size(d2))

    open (1, file='decEditDesc008.data', sign='suppress')

    do l = 1, 10
        do k = 1, 10
            do j = 1, 10
                do i = 1, 10
                    write (1, '(sp, dp, e30.16)') -d2(i,j,k,l)
                end do
            end do
        end do
    end do

    iunit = newUnit (1)

    do i = 1, size(d1)/2
        read (iunit, '(dc, d30.16)') d1(i)
    end do

    open (iunit, decimal='point')
    open (iunit, decimal='comma ')

    do i = size(d1)/2+1, size(d1)
        read (iunit, '(ss, e30.16)') d1(i)
    end do


    !! verify the results of d1

    ncount = 1

    do l = 1, 10
        do k = 1, 10
            do j = 1, 10
                do i = 1, 10
                    if (.not. precision_r8(d2(i,j,k,l), d1(ncount))) &
                        error stop 20_4

                    ncount = ncount + 1
                end do
            end do
        end do
    end do
end


integer function newUnit (unit)
    integer, intent(in) :: unit

    character(2000) line
    integer stat

    stat = 0

    newUnit = unit*10

    open (newUnit, file='tempFile')

    rewind (unit)

    do while (stat == 0)
        read (unit, '(a)', pad='yes', iostat=stat) line

        if (stat /= 0) exit

        line(index(line, '-'): index(line, '-')) = '+'
        line(index(line, '.'):index(line, '.')) = ','

        write (newUnit, '(a)') trim(line)
    end do

    close (newUnit)
    close (unit)

    newUnit = unit

    open (unit, file='tempFile', status='old', position='rewind')
end function
