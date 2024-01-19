! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/14/2005
!*
!*  DESCRIPTION                : final sub (finalization of temps created by
!                               structure constructor in write statement)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(4), allocatable :: data(:)

        contains

        final :: finalizeBase
    end type

    interface write (formatted)
        subroutine writeBaseFmt (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            integer, intent(in) :: unit
            class (base), intent(in) :: dtv
            character(*),intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        if (allocated (b%data)) then
            print *, 'deallocate data'

            deallocate (b%data)
        end if
    end subroutine
end module

program ffinal515a4_1
use m
    integer stat1
    character(200) err

    write (*,*) base (null())

    write (*, *) base ((/2.2, 4.2, 3.1/))

    print *, 'end'
end


!! we only handle the list-directed write
subroutine writeBaseFmt (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    integer, intent(in) :: unit
    class (base), intent(in) :: dtv
    character(*),intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 10_4

    if (allocated (dtv%data)) then
        write (unit, '(5f10.2)', iostat=iostat, iomsg=iomsg) dtv%data
    else
        write (unit, *, iostat=iostat, iomsg=iomsg) 'data not allocated'
    end if

end subroutine
