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


program decEditDesc008
use m
    logical, external :: precision_r8

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

    write (1, *) ''

    close(1)

    call changeFileOnPlace ('decEditDesc008.data  ')

    open (1, file='decEditDesc008.data', decimal='point ')

    do i = 1, size(d1)/2
        read (1, '(dc, d30.16)') d1(i)
    end do

    open (1, decimal='point')
    open (1, decimal='comma ')

    do i = size(d1)/2+1, size(d1)
        read (1, '(e30.16)') d1(i)
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


subroutine changeFileOnPlace (fileName)
    character(*), intent(in) :: fileName

    integer currPos, stat
    character(2000) line

    stat = 0

    open (10, file=fileName, action='readwrite', form='formatted', &
            access='stream', decimal='Comma')

    do while (stat == 0)
        inquire (10, pos=currPos)

        read (10, '(a)', pad='yes', iostat=stat) line

        if (stat /= 0) exit

        line(index(line, '-'): index(line, '-')) = '+'
        line(index(line, '.'):index(line, '.')) = ','

        write (10, '(a)', pos=currPos) trim(line)
    end do

    close(10)
end subroutine
