!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio501a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (arrays in DTIO in a select
!                               type construct)
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
        integer(4), pointer :: i
    end type

    integer(4), parameter :: ISNULL = -999999
end module


program fdtio501a
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

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    class(*), allocatable :: b1(:), b2(:)
    integer stat
    character(200) :: errormsg

    integer(4), target :: i2 = 200
    integer(4) i3

    allocate (b1(0:1), source=(/base(i=null()), base(i=i2)/))

    open (1, file='fdtio501a.data', form='unformatted')

    select type (b1)
        class is (base)
            write (1) b1, -1000

        class default
            error stop 10_4
    end select

    allocate (base:: b2(2))


    rewind (1)

    select type (b2)
        class is (base)
            read(1, iostat=stat, iomsg=errormsg) b2, i3

            if (stat /= 0) error stop 4_4

            if (associated(b2(1)%i)) error stop 1_4
            if (b2(2)%i /= 200) error stop 2_4

            if (i3 /= -1000) error stop 3_4

        class default
            error stop 11_4
    end select

    close(1, status='delete')
end


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) temp, err

    !! we do not care about the failure of deallocation
    if (associated (dtv%i))   deallocate (dtv%i, stat=err)

    read (unit, iostat=iostat, iomsg=iomsg) temp

    if (iostat /= 0) return

    !! check for NULL pointer
    if (temp == ISNULL) then
        nullify (dtv%i)
    else
        allocate (dtv%i, source=temp)
    end if
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated (dtv%i)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%i
    else
        write (unit, iostat=iostat, iomsg=iomsg) ISNULL
    end if
end subroutine

