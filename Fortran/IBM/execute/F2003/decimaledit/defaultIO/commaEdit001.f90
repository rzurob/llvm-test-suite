!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/19/2006
!*
!*  DESCRIPTION                : decimal edit mode
!                               Test that the IO on the same connection follows
!                               the decimal edit mode set in OPEN statement;
!                               write statement on the same unit across multiple
!                               scoping units.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    real r1, r2(10)
    real(8), allocatable :: d1(:)
end module

subroutine writeDataFedit (unit, fmt)
use m
    integer, intent(in) :: unit
    character(*), intent(in) :: fmt

    character(len(fmt)+5) Ffmt

    Ffmt = '(6f'//fmt//')'

    write(unit, Ffmt) r1, r2

    if (allocated(d1)) then
        write (unit, '(2F'//fmt//')', SIGN="PLUS") d1
    end if
end subroutine

subroutine writeDataEedit (unit, fmt)
use m
    integer, intent(in) :: unit
    character(*), intent(in) :: fmt

    integer w, d
    character(len(fmt)+5) localFmt

    read (fmt, *, decimal='COMMA') w,d

    write(localFmt,'(i3,a,i2)') w, '.',d

    write (unit, '(2E'//localFmt//")", advance='no') r1, r2

100 format (3E<w>.<d>)

    if (allocated(d1)) then

        write (unit, 100, iostat=i1) d1(:)

        if (i1 /= 0) error stop 10_4
    end if
end subroutine

program commaEdit001
    open (1, file='commaEdit1.out', form='formatted', decimal='COMMA')

    call setupModData

    call writeDataFedit (1, '10.3')

    call writeDataEedit (1, '12;3')

    open (1, file='commaEdit2.out', form='formatted')

    call writeDataFedit (1, '10.3')

    call writeDataEedit (1, '12;3')
end

subroutine setupModData
use m
    r1 = 1.0
    r2 = asin((/(3.0e-2*i,i=1,10)/))

    if (.not. allocated(d1)) allocate(d1(10))

    d1 = sin(r1*r2)
end
