! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/30/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Inquire statement on unit during the child data
!                               transfer; use internal file; test both decimal=
!                               specifier and DC/DP edit descriptor in parent
!                               WRITE statement; for DTIO on internal file, the
!                               unit is set to -1 and the INQUIRE stmt will
!                               return UNDEFINED for decimal edit mode (treated
!                               as if no connection is made).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type stringStack
        character(:), allocatable :: str(:)
        integer(4) :: slots = 0, top = 0, len=5

        contains

        procedure :: push => pushString2Stack
    end type

    type(stringStack), save :: modeStack

    type base
        real(4), allocatable :: data(:)
        logical, allocatable :: flag

        contains

        procedure :: writeBaseFmt
        generic :: write (formatted) => writeBaseFmt
    end type

    contains

    subroutine pushString2Stack (ss, str)
        class(stringStack), intent(inout) :: ss
        character(*), intent(in) :: str

        character(:), allocatable :: localStrStore(:)

        ss%len = max(ss%len, len(trim(str)))

        !! if the input string is longer than all strings, then allocate the
        !ss%str to be the new length
        if (allocated(ss%str) .and. (ss%len > len(ss%str))) then
            allocate (character(ss%len) :: localStrStore(ss%slots))

            localStrStore(:) = ss%str

            call move_alloc (localStrStore, ss%str)
        end if

        !! if we're sitting on the top, then we need allocate more slots
        if (ss%top >= ss%slots) then
            ss%slots = ss%slots + 10

            allocate (character(ss%len) :: localStrStore(ss%slots))

            localStrStore(:ss%slots-10) = ss%str

            call move_alloc (localStrStore, ss%str)
        end if

        ss%str(ss%top+1) = str

        ss%top = ss%top + 1
    end subroutine

    subroutine writeBaseFmt (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(:), allocatable :: decMode

        allocate (character(20) :: decMode)

        inquire(unit, decimal=decMode, iostat=itest)

        if (itest /= 0) decMode = 'UNDEFINED'

        call modeStack%push (decMode)
    end subroutine
end module

program dcmlChildWrite003
use m
    character(20) :: string1(10)
    class (base), allocatable :: b1(:)

    namelist /nml1/ b1

    string1 = ''
    allocate (b1(20))

    write (string1(1), '(dc, DT)') b1(1)

    write (string1(2), *, decimal='Comma    ') b1(::2)

    write (string1(3), '(DT)', decimal='Point') b1(2::2)

    write (string1(5:), nml1, decimal='comma')

    if (modeStack%slots /= 50) error stop 1_4

    if (modeStack%top /= 41) error stop 2_4

    if (modeStack%len /= 9) error stop 3_4

    if (any (modeStack%str(:41) /= 'UNDEFINED')) error stop 4_4

    if (any (string1((/1,3,4,8,9,10/)) /= '')) error stop 5_4

    if (string1(2) /= ' ') error stop 6_4

    if (any (string1(5:7) /= (/' &NML1', ' B1=  ', ' /    '/))) error stop 7_4
end
