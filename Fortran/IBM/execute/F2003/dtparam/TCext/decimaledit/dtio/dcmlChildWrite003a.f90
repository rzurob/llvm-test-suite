! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=self /tstdev/F2003/decimaledit/dtio/dcmlChildWrite003a.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

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
!*  DATE                       : 06/29/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Use the same structure as in dcmlChildWrite003
!                               but test units connected to external files;
!                               decimal edit mode in inquire statement in child
!                               data transfer set by OPEN or parent WRITE
!                               statement.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type stringStack(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        character(:), allocatable :: str(:)
        integer(k1)   :: slots = 0, top = 0, len=5

        contains

        procedure :: push => pushString2Stack
    end type

    type(stringStack(20,4)), save :: modeStack

    type base(n2,k2)    ! (20,4)
        integer, kind            :: k2
        integer, len             :: n2
        real(k2), allocatable    :: data(:)
        logical(k2), allocatable :: flag

        contains

        procedure :: writeBaseFmt
        generic :: write (formatted) => writeBaseFmt
    end type

    contains

    subroutine pushString2Stack (ss, str)
        class(stringStack(*,4)), intent(inout) :: ss
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
        class(base(*,4)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(20) decMode

        inquire(unit, decimal=decMode)

        call modeStack%push (decMode)
    end subroutine
end module

program dcmlChildWrite003a
use m
    character(200) :: string1
    class (base(:,4)), allocatable :: b1(:)

    namelist /nml1/ b1

    allocate (base(20,4) :: b1(20))

    write (1, *) b1(1)

    write (1, *, decimal='Comma    ') b1(::2)

    write (1, '(DT)', decimal='Point') b1(2::2)

    write (1, nml1, decimal='comma', iostat=i1)

    open (2, file='test', decimal='comMA')

    write (2, *) b1

    write (2, '(DP, DT)') b1

    if (modeStack%top /= 81) error stop 1_4

    if (modeStack%slots /= 90) error stop 2_4

    if (modeStack%len /= 5) error stop 3_4

    if (modeStack%str(1) /= 'POINT') error stop 4_4

    if (any (modeStack%str(2:11) /= 'COMMA')) error stop 5_4

    if (any (modeStack%str(12:21) /= 'POINT')) error stop 6_4

    if (any (modeStack%str(22:41) /= 'COMMA')) error stop 7_4

    if (any (modeStack%str(42:61) /= 'COMMA')) error stop 8_4

    if (any (modeStack%str(62:81) /= 'POINT')) error stop 9_4
end
