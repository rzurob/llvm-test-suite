! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/31/2005
!*
!*  DESCRIPTION                : DTIO generics (test the runtime error that
!                               raised as user tries to do DTIO on types that
!                               requires DT edit descriptor which is NOT
!                               supplied)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
    end type

    type, extends(base) :: child
        character(20), private :: name
    end type


    interface read(formatted)
        module procedure formattedRead
    end interface

    contains

    !! does NOT take read other than list-directed
    subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        class (base), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') error stop 11
    end subroutine

    subroutine writeB (b)
        class (base), intent(in) :: b

        select type (b)
            type is (base)
                print *, b%id
            type is (child)
                print *, b%id, b%name
        end select
    end subroutine
end module

program fdtio519b1
use m
    class (child), allocatable :: b1(:,:)

    allocate (child:: b1(2,2))

    open (1, file='fdtio519b1.in')

    write (1, '(i5,a20)') 100, 'xlftest 01'

    rewind 1

    read (1, '(i5,a20)') b1(1,1)

    close (1, status='delete')
end
