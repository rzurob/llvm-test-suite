! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  DTIO
!*                                         CLASS(derived-type-spec) may specify an abstract type
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m
   type, abstract:: base(k1)
      integer, kind :: k1
      integer(k1) i
   end type

   type, extends(base) :: child(k2)
      integer, kind :: k2
   end type

end module

program dtio002
   use m

   interface read(unformatted)
         subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import base
            class(base(4)), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    class(base(4)), allocatable :: b1
    class(base(4)), pointer :: b2
    integer :: iostat
    character(20):: iomsg

    open (1, form='unformatted', status='scratch')

    allocate (b1, source = child(4,4)(10) )
    allocate (child(4,4) :: b2)

    write (1, iostat=iostat, iomsg=iomsg ) b1
    if (iostat .ne. 0) error stop 1_4

    backspace 1
    read  (1) b2

    if (b2%i .ne. 10) error stop 2_4

    close (1)

end program

subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class(base(4)), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) :: temp
    read(unit, iostat=iostat, iomsg=iomsg) temp
    dtv%i = temp
end subroutine

subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg) dtv%i
end subroutine
