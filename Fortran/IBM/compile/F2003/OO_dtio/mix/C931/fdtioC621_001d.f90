! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/4/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (C931 and C621)
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
        integer(4), pointer :: i => null()
    end type
end module

program fdtioC621_001d
use m
    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type (base) b1(10)

    call xyz (b1(1), b1(2:10))

    contains

    subroutine xyz(a, b)
        class (base), intent(inout) :: b(*)
        class (base), intent(inout) :: a

        read (1) a, b(1::2)     !<-- illegal
        read (1) b(5)           !<-- this is legal
    end subroutine
end
