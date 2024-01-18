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
! %GROUP: fdtio517a.f
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
!*  DATE                       : 01/11/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO generics (formatted stream read for
!                               pad='no' and advance='no')
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
        real, pointer :: data(:) => null()
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


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character openPar, closePar

    if (iotype /= 'LISTDIRECTED') error stop 10_4

    if (size(v_list) /= 0) error stop 11_4

    allocate (dtv%data(2))

    !! child data transfer is always no-advancing IO
    read (unit, '(2(a1,g8.2,a1))', iostat=iostat, iomsg=iomsg) &
            (openPar, dtv%data(i), closePar, i=1,2)
end subroutine


program fdtio517a
use m
use iso_fortran_env

    class (base), allocatable :: b1(:)

    integer stat1
    character(200) err

    logical precision_r4

    allocate (b1(2))

    open (1, access='stream', form='formatted', pad='no')

    write (1, pos=1, fmt='(3(a1,g8.2,a1), a,a)') '(', 10.2, ')', '(', 3.5, ')', &
                    '(', -1.3, ')', '(1.3)', new_line('a')


    !! EOR should happen during this read for b1(2)
    read (1, *, pos=1, iostat=stat1, iomsg=err) b1

    if (stat1 /= iostat_eor) then
        print *, stat1, err
        error stop 1_4
    end if

    !! verify b1(1) is read in
    if (.not. precision_r4(b1(1)%data(1), 10.0_4)) error stop 2_4

    if (.not. precision_r4(b1(1)%data(2), 3.5_4)) error stop 3_4

    close(1, status='delete')
end
