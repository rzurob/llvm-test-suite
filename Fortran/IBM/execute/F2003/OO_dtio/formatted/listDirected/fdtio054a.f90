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
! %GROUP: fdtio054a.f
! %VERIFY: fdtio054a.out:fdtio054a.vf
! %STDIN:
! %STDOUT: fdtio054a.out
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
!*  DATE                       : 01/04/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO generics (internal file IO always allowed
!                               during DTIO)
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
        class (*), allocatable :: data(:)

        contains

        procedure :: print => printBase
    end type

    character(100) globalBuffer

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

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        if (allocated (b%data)) then

            select type (x => b%data)
                type is (integer)
                    print *, 'Integer data'
                    do i = lbound(x,1), ubound(x,1)
                        print *, x(i)
                    end do
                type is (real)
                    print *, 'Real data'

                    do i = lbound(x,1), ubound(x,1)
                        print '(f10.2)', x(i)
                    end do

                type is (character(*))
                    print *, 'Character type'

                    do i = lbound(x,1), ubound(x,1)
                        print *, x(i)
                    end do
            end select
        end if
    end subroutine
end module

program fdtio054a
use m
    class (base), allocatable :: b1

    integer stat
    character(200) err

    open (1, file='fdtio054a.data')

    allocate (b1)

    write (1, *) 1.0, 2.1, 1.5

    write (1, *) 10, 20

    write (1, '(a)') 'abc xyz 3 4 5'

    rewind 1

    !!
    !! test the 1st string
    !!
    write (globalBuffer, '(a)') 'real 3'

    read (1, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    !! verify the data
    if (size(b1%data) /= 3) error stop 2_4

    call b1%print

    !!
    !! test the 2nd string
    !!
    write (globalBuffer, '(a)') 'int 2'

    read (1, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err 
        error stop 3_4
    end if

    !! 
    if (size(b1%data) /= 2) error stop 4_4

    call b1%print

    !!
    !! test the 3rd string
    !!
    write (globalBuffer, '(a)') 'char 11'

    read (1, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 5_4
    end if

    if (size(b1%data) /= 11) error stop 6_4

    call b1%print

    close (1, status='delete')
end


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, globalBuffer
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(4) dataType
    integer(4) dataSize

    if (allocated (dtv%data)) deallocate (dtv%data)

    read (globalBuffer, *, iostat=iostat, iomsg=iomsg) dataType, dataSize

    if (iostat /= 0) return

    if (dataType == 'real') then
        allocate (real :: dtv%data(dataSize))
    else if (dataType == 'int') then
        allocate (integer :: dtv%data(dataSize))
    else
        allocate (character :: dtv%data(dataSize))  !<-- by default treat as string
    end if

    select type (x => dtv%data)
        type is (integer)
            read (unit, *, iostat=iostat, iomsg=iomsg) x
        type is (real)
            read (unit, *, iostat=iostat, iomsg=iomsg) x
        type is (character(*))
            do i = 1, size(x)
                read (unit, '(a)', iostat=iostat, iomsg=iomsg) x(i)
                if (iostat /= 0) return
            end do
        class default
            error stop 10_4
    end select
end subroutine
