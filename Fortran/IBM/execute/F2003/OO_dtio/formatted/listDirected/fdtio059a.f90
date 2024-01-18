!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio059a.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 01/13/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO generics (effect of TR and TL during DTIO)
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

module m
    type base
        integer(8), allocatable :: data(:)
    end type


    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


!! read in 3 integer data
subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

!    if (iotype /= 'LISTDIRECTED') return

    if (size (v_list) /= 0) error stop 10_4

    if (allocated (dtv%data)) then
        if (size (dtv%data) /= 3) deallocate (dtv%data)
    end if

    if (.not. allocated (dtv%data)) allocate (dtv%data(3))

    !! read the data in reverse order
    read (unit, '(TR20,i10,TL20,i10,TL30,i10,TR20)', iostat=iostat, iomsg=iomsg) &
                        dtv%data
end subroutine


program fdtio059a
use m
    class (base), pointer :: b1(:)

    integer stat1
    character(200) err

    allocate (b1(2))

    allocate (b1(2)%data(3))

    write (1, '(6i10)') (i, i=1,12,2)

    rewind (1)

    !! test read a scalar
    read (1, *, iostat=stat1, iomsg=err) b1(1)

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 1_4
    end if

    if (any (b1(1)%data /= (/5_8,3_8,1_8/))) error stop 4_4

    !! test read for an array
    rewind 1
    read (1, '(2DT)', iostat=stat1, iomsg=err) b1

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 2_4
    end if

    if (any (b1(1)%data /= (/5_8,3_8,1_8/))) error stop 5_4

    if (any (b1(2)%data /= (/11_8,9_8,7_8/))) error stop 6_4
end
