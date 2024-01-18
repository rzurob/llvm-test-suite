!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 03/08/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (multiple components handled by
!                               DTIO)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base1
        real, pointer :: data(:)
    end type

    type base2
        complex, allocatable :: data(:)
    end type


    interface write(formatted)
        subroutine formattedWriteBase1 (dtv, unit, iotype, v_list, iostat, iomsg)
        import base1
            class (base1), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine formattedWriteBase2 (dtv, unit, iotype, v_list, iostat, iomsg)
        import base2
            class (base2), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


!! only deal with list-directed write
subroutine formattedWriteBase1 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base1
    class (base1), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 11_4

    if (associated (dtv%data)) then
        write (unit, '(7g10.2)', iostat=iostat, iomsg=iomsg) dtv%data

        if (iostat == 0) write (unit, '(a,/)', iostat=iostat, iomsg=iomsg) ''
    else
        write (unit, '(a,/)', iostat=iostat, iomsg=iomsg) 'NULL'
    end if
end subroutine

subroutine formattedWriteBase2 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base2
    class (base2), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 21_4

    if (allocated (dtv%data)) then
        write (unit, 100, iostat=iostat, iomsg=iomsg) dtv%data

        if (iostat == 0) write (unit, '(a,/)', iostat=iostat, iomsg=iomsg) ''
    else
        write (unit, '(a,/)', iostat=iostat, iomsg=iomsg) 'NULL'
    end if

!! NOTE: the statement 100 is IBM extension: use of variable repetition
100 format (<size(dtv%data)>(1x, '(', g9.2, ',', g9.2, ')'))
end subroutine


module n
use m
    type dataType
        type (base1) :: b1(2)
        type (base2) :: b2(0:1)
    end type
end module


program fdtio518a
use m
use n
    type (dataType) :: d1(2)

    allocate (d1(1)%b1(1)%data(2), source=(/1.0, 2.0/))
    allocate (d1(1)%b1(2)%data(1), source=(/1.5/))

    allocate (d1(1)%b2(0)%data(-1:0), source= (/(3.0, 2.0), (4.0, 2.0)/))
    allocate (d1(1)%b2(1)%data(1), source= (/(3.5, 2.5)/))

    nullify(d1(2)%b1(1)%data, d1(2)%b1(2)%data)

    allocate (d1(2)%b2(0)%data(3), source=(/(1.3, 2.5), (2.3, 465.), (12.,1.2)/))
    allocate (d1(2)%b2(1)%data(1), source=(/(1.7, 4.5)/))

    !! test the write for scalar
    print *, d1(1)

    !! test the write for array
    print *, d1
end
