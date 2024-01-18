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
!*  DATE                       : 05/31/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (DTIO generics in local scope)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        private
        complex(8) :: data
        integer id
    end type

    type, extends(base) :: child
        private
        character(20) :: name
    end type

    private formattedWrite

    contains

    subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        class (base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') return

        if (size(v_list) /= 0) error stop 10_4

        select type (dtv)
            type is (base)
                write (unit, *, iostat=iostat, iomsg=iomsg) dtv
            type is (child)
                write (unit, *, iostat=iostat, iomsg=iomsg) dtv
            class default
                error stop 11_4
        end select
    end subroutine

    subroutine setVal (b, data, id, name)
        class (base), intent(out) :: b
        complex(8), intent(in) :: data
        integer, intent(in) :: id
        character(*), intent(in) :: name

        select type (b)
            type is (base)
                b = base (data, id)
            type is (child)
                b = child (data, id, name)
            class default
                error stop 15_4
        end select
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        interface write(formatted)
            module procedure formattedWrite
        end interface

        print *, b
    end subroutine
end module

program fdtio524a1
use m
    class (base), pointer :: b1(:)

    allocate (child::b1(2))

    call setVal (b1(1), (1.0_8, 2.0_8), 10, 'test xlf')

    call printBase (b1(1))
end
