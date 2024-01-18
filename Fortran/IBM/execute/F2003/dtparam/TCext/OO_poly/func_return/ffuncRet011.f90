! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet011.f
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
!*  DATE                       : 05/09/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly function return (DTIO with poly-function
!                               return)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind        :: k1
        integer(k1), pointer :: id => null()

        contains

        final :: finalizeBase
    end type

    interface write(formatted)
        module procedure formattedWrite
    end interface

    interface base
        module procedure produceBaseAlloc
    end interface

    contains

    !! this routine only deals with list-directed write
    subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        class (base(8)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') return

        if (size(v_list) /= 0) error stop 10_4

        if (associated (dtv%id)) then
            write (unit, '(i15)', iostat=iostat, iomsg=iomsg) dtv%id

            if (iostat /= 0) return
        end if
    end subroutine

    subroutine finalizeBase (b)
        type (base(8)), intent(inout) :: b

        if (associated(b%id)) then
            print *, 'deallocating id'

            deallocate (b%id)
        end if
    end subroutine

    class(base(8)) function produceBaseAlloc (id)
        integer(8), intent(in) :: id

        allocatable produceBaseAlloc

        allocate(produceBaseAlloc)
        allocate (produceBaseAlloc%id, source=id)
    end function
end module

program ffuncRet011
use m
    print *, base(10_8)

    print *, (/base(1_8), base(12_8)/)
    print *, 'end'
end
