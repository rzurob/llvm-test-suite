!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/4/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (use of part of the
!                               assumed-size array in defined read statement)
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

    type base1
        integer(4) :: i
    end type
end module

program fdtio001
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

    type (base) b1(11)

    write (1) base1(1), (base1(j), j=2,10,2)

    rewind(1)

    call xyz (b1(1), b1(2:10))

    if (b1(1)%i /= 1) error stop 1_4

    do i = 2, 10, 2
        if (b1(i)%i /= i) error stop 2_4
        if (associated (b1(i+1)%i)) error stop 3_4
    end do

    contains

    subroutine xyz(a, b)
        class (base), intent(inout) :: b(*)
        class (base), intent(inout) :: a

        read (1) a, b (1:9:2)
    end subroutine
end


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) temp, stat1

    read (unit, iostat=iostat, iomsg=iomsg) temp

    if (associated (dtv%i)) deallocate (dtv%i, stat=stat1)

    allocate (dtv%i, source=temp)
end subroutine
