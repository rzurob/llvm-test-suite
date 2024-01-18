! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/decimaledit/dtio/modeInquire004.f
! opt variations: -qck -qnok -ql -qdefaultpv -qreuse=none

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
!*  DATE                       : 07/25/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test during child unformatted write statement an
!                               INQUIRE statement for decimal= on the unit
!                               returns value of UNDEFINED.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type node(k1)    ! (4)
        integer, kind           :: k1
        character(:), allocatable :: name
        type(node(k1)), pointer :: next => null()

        contains

        procedure :: writeNodeUFmtd
        generic :: write(unformatted) => writeNodeUFmtd

        final :: finalizeNode
    end type

    type linkedList(k2)    ! (4)
        integer, kind           :: k2
        type(node(k2)), pointer :: head => null()

        contains

        procedure :: writeLListUFmtd
        generic :: write(unformatted) => writeLListUFmtd

        final :: finalizeLList
    end type

    contains

    recursive subroutine finalizeNode (n)
        type(node(4)), intent(inout) :: n

        if (associated(n%next)) deallocate(n%next)
    end subroutine


    subroutine finalizeLList (ll)
        type(linkedList(4)), intent(inout) :: ll

        if (associated(ll%head)) deallocate(ll%head)
    end subroutine


    recursive subroutine writeNodeUFmtd (dtv, unit, iostat, iomsg)
        class(node(4)), intent(in) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(:), allocatable, save :: localTemp

        localTemp = repeat(' ', 9)

        inquire(unit, decimal=localTemp, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        if (allocated(dtv%name)) then
            write (unit, iostat=iostat, iomsg=iomsg) dtv%name
        end if

        if (iostat /= 0) return

        write (unit, iostat=iostat, iomsg=iomsg) localTemp

        if (iostat /= 0) return

        if (associated(dtv%next)) then
            write (unit, iostat=iostat, iomsg=iomsg) dtv%next
        end if
    end subroutine

    subroutine writeLListUFmtd (dtv, unit, iostat, iomsg)
        class(linkedList(4)), intent(in) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (associated(dtv%head)) &
            write(unit, iostat=iostat, iomsg=iomsg) dtv%head
    end subroutine
end module

program modeInquire004
use m
    type(linkedList(4)) :: list

    allocate(list%head)
    list%head = node(4)('comma')

    allocate(list%head%next)
    list%head%next = node(4)('point')

    allocate(list%head%next%next)
    list%head%next%next = node(4)('unknown')

    open (1, access="stream")

    write (1) list
end
