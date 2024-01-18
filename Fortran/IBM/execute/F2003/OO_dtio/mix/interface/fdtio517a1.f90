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
! %GROUP: fdtio517a1.f
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
!*  DATE                       : 01/18/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO generics (non-advancing DTIO on internal
!                               file should fail)
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
        character (10), allocatable :: data(:)
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


!! this subroutine only treats the list-directed read
subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') error stop 10_4

    if (size (v_list) /= 0) error stop 11_4

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (dtv%data(2))

    read (unit, '(2a10)', advance='no', iostat=iostat, iomsg=iomsg) dtv%data
end subroutine


program fdtio517a1
use m
use iso_fortran_env
    class (base), allocatable :: b1(:)

    integer stat1
    character(200) err
    logical precision_r8

    character (30) c1

    allocate (b1(3))

    write (c1, '(a10,a)') '0123456789', new_line('a')
    write (c1(12:), '(a10, a8)') '9876543', 'abcdef'

    !! this read statement will fail as DTIO fails due to EOF
    read (c1(12:), *, iostat=stat1, iomsg=err) b1(1)

    if (stat1 /= IOSTAT_EOR) then
        print *, stat1, err
        error stop 1_4
    end if

    !! similarly the read for array b1 will fail
    read (c1, *, iostat=stat1, iomsg=err) b1

    if (stat1 /= IOSTAT_EOR) then
        print *, stat1, err
        error stop 2_4
    end if

end
