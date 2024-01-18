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
!*  DATE                       : 01/06/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type parameter in dtv must be
!                               init-expr.: generic interface. (one case for
!                               C402)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        real(k), allocatable :: data(:)
    end type

    integer, parameter :: singlePrec = 4, doublePrec = 8

    interface write(formatted)
        subroutine formattedWriteBase4(dtv, unit, iotype, v_list, iostat, iomsg)
        import
            class (base(singlePrec)), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine formattedWriteBase8(dtv, unit, iotype, v_list, iostat, iomsg)
        import
        class (base(doublePrec)), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


!! deal with list-directed write for single precision
subroutine formattedWriteBase4(dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, singlePrec
    class (base(singlePrec)), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) then
        do i = lbound(dtv%data, 1), ubound(dtv%data, 1)
            write (unit, '(f10.2, a)', iostat=iostat, iomsg=iomsg) dtv%data(i), ', '

            if (iostat /= 0) return
        end do

        write (unit, '(/)', iostat=iostat, iomsg=iomsg)
    end if
end subroutine


!! deal with list-directed write for double precision
subroutine formattedWriteBase8(dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, doublePrec
    class (base(doublePrec)), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) then
        do i = lbound(dtv%data, 1), ubound(dtv%data, 1)
            write (unit, '(g18.5, a)', iostat=iostat, iomsg=iomsg) dtv%data(i), ', '
            if (iostat /= 0) return
        end do

        write (unit, '(/)', iostat=iostat, iomsg=iomsg)
    end if
end subroutine

program kindparamDTIO001
use m
    class(base(singlePrec)), allocatable :: b1(:)

    type (base(doublePrec)) :: b2(3)

    allocate (b1(5))

    allocate (b1(1)%data(2), source=(/1.2e0, 2.2e0/))

    allocate (b1(3)%data(5), source=(/(i*2.1e0, i=1, 5)/))

    allocate (b2(1)%data(3), source=(/3.22d55, -1.2d49, 3.33d-89/))
    b2(2) = b2(1)
    b2(2)%data = b2(2)%data(3:1:-1)

    !! invoke the IO
    print *, b1
    print *, b2
end
