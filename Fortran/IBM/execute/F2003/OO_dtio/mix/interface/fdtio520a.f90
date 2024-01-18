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
!*  DESCRIPTION                : DTIO generics (list-directed read operation for
!                               a linked-list)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type node
        real(8) value
        type (node), pointer :: next => null()

        contains

        procedure :: print => formattedPrintNode
    end type

    interface read(formatted)
        module procedure formattedRead
    end interface

    contains

    !! assume there is at least one node's data for read
    recursive subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        class (node), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(15) c1

        !! we only handles the DT edit descript
        if (iotype(1:2) /= 'DT') return

        if (size(v_list) /= 0) error stop 10_4

        read (unit, '(a15)', iostat=iostat, iomsg=iomsg) c1

        if (iostat /= 0) return

        read (c1, '(g15.2)', iostat=iostat, iomsg=iomsg) dtv%value

        if (iostat /= 0) return

        !! peek next character
        read (unit, '(a1,TL1)', iostat=iostat, iomsg=iomsg) c1(1:1)

        !! '/' is a marker tells the program to terminate the read
        if (c1(1:1) == '/') return

        allocate (dtv%next)

        !! keep reading
        read (unit, '(DT)', iostat=iostat, iomsg=iomsg) dtv%next
    end subroutine

    subroutine formattedPrintNode (n, fmt)
        class (node), intent(in) :: n
        character(*), intent(in) :: fmt

        type (node), pointer :: iterator

        write (*, fmt) n%value

        iterator => n%next

        do while (associated (iterator))
            write (*, fmt) iterator%value

            iterator => iterator%next
        end do
    end subroutine
end module


program fdtio520a
use m
    type (node) :: list

    integer stat1
    character(200) err

    write (1, '(5g15.2, a1)') (j*1.5, j = 1, 5), '/'

    rewind (1)

    read (1, '(DT)') list

    call list%print ("(f10.2)")

    close (1, status='delete')
end
