! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/23/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Use of semicolon as the value separator for
!                               list-directed input; update output file
!                               previously created.
!                               Similar case to decEditDesc008 but with
!                               different approach: read in everything first.
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
    logical(4), external :: precision_r8
    integer indices(4)

100 format (sp, dp, 4(i5,","), e30.17)

    allocate (d2(10,10,10,10))

    allocate (d1(size(d2)))

    call setRandomValues (d2, size(d2))

    open (1, file='valueSeparator.data', sign='suppress')

    do k = 10, 1, -1
        do l = 1, 10
            do i = 1, 10
                do j = 1, 10
                    write (1, 100) i,j,k,l, d2(i,j,k,l)
                end do
            end do
        end do
    end do

    call changeFileMode (1)

    rewind 1

    do i = 1, size(d1)/2
        read (1, *, decimal='coMMa') indices, &
            d1((indices(4)-1)*1000+(indices(3)-1)*100+(indices(2)-1)*10+indices(1))
    end do

    open (1, decimal='point')
    open (1, decimal='comma ')

    do i = size(d1)/2+1, size(d1)
        read (1, *) indices, &
            d1((indices(4)-1)*1000+(indices(3)-1)*100+(indices(2)-1)*10+indices(1))
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


subroutine changeFileMode (unit)
    integer, intent(in) :: unit

    character(:), allocatable :: pages(:)
    integer stat, totalLines, maxLenWidth, indexComma, indexPoint

    character(1000) c

100 format (a)

    stat = 0
    totalLines = 0
    maxLenWidth = 1

    rewind (unit)

    do while (stat == 0)
        read(unit, '(a)', iostat=stat) c

        if (stat /= 0) exit

        totalLines = totalLines + 1
        maxLenWidth = max(maxLenWidth, len(trim(c)))
    end do

    allocate (character(maxLenWidth) :: pages(totalLines))

    rewind (unit)

    read (unit, 100) pages

    do i = 1, totalLines
        indexComma = 1
        indexPoint = 1

        do while (indexComma /= 0)
            indexComma = index(pages(i), ',')

            if (indexComma == 0) exit

            pages(i)(indexComma:indexComma) = ';'
        end do

        do while (indexPoint /= 0)
            indexPoint = index(pages(i), '.')

            if (indexPoint == 0) exit

            pages(i)(indexPoint:indexPoint) = ','
        end do
    end do

    rewind (unit)

    write (unit, 100) pages
    rewind unit
end subroutine
