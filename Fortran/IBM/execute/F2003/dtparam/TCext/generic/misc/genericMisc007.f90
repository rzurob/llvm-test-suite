! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/misc/genericMisc007.f
! opt variations: -qnol

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : misc.
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : 
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
    type base(n1,k1)    ! (20,8)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)      :: r1(12)
        procedure(printVal), pointer, pass(b) :: print

        contains

        procedure :: formattedWriteBase
        generic :: write(formatted) => formattedWriteBase
    end type

    abstract interface
        integer function printVal (unit, b, fmt)
        import
            integer, intent(in) :: unit
            class(base(*,8)), intent(in) :: b
            character(*), intent(in) :: fmt
        end function

        subroutine writeBase (b, unit, iotype, vlist, iostat, iomsg)
        import
            class(base(*,8)), intent(in) :: b
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    procedure(writeBase) formattedWriteBase
end module


subroutine formattedWriteBase (b, unit, iotype, vlist, iostat, iomsg)
use m, only: base
    class(base(*,8)), intent(in) :: b
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(30) fmt

    if (iotype == 'LISTDIRECTED') then
        fmt = '(5g10.4)'
    else if (iotype(1:2) == 'DT') then
        write(fmt, '(a,i5,a,i5,a)') '(5g', vlist(1),'.', vlist(2), ')'
    else
        iostat = 100
        return
    end if

    if (associated(b%print)) then
        iostat=b%print(unit, fmt)
    else
        iostat = 200
    end if
end subroutine

program genericMisc007
use m
    type (base(20,8)) b1(10)

    character(30) fmt

    procedure(printVal) printBaseVal

    b1 = (/(base(20,8)((/(100*j+i, i=1,12)/), printBaseVal), j = 1, 10)/)

    read *, iunit

    read *, i1, i2

    write (fmt, *) "(DT'base type'(", i1, ",", i2,"))"
    write(iunit, fmt=fmt) b1

    i3 = b1(6)%print(6, '(g12.5)')
end


integer function printBaseVal (unit, b, fmt)
use m
    integer, intent(in) :: unit
    class(base(*,8)), intent(in) :: b
    character(*), intent(in) :: fmt

    write(unit, fmt=fmt, iostat=printBaseVal) b%r1
end function
