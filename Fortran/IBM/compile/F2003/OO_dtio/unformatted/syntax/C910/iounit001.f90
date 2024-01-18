! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5 Data Transfer Statement
!*                               C910: io-unit shall be specified, if UNIT omitted, first argument shall be io-unit
!*                               - do not specify io-unit
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base
      character(3) :: c
      contains
         procedure, pass :: getC
         procedure, pass :: setC
   end type
contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function

   subroutine setC (a, char)
      class(base), intent(inout) :: a
      character(3), intent(in) :: char
      a%c = char
   end subroutine
end module


program iounit001
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base),  intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout):: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(100) :: msg

   class(base), allocatable :: b1
   class(base), pointer :: b2

   allocate (b1, source = base("ibm") )
   allocate (b2, source = base("IBM") )

   open (2, file="iounit001.udata", form="unformatted", access="sequential" )

   write (iomsg=msg) b1                !<= unit is missing
   write (2) b1
   rewind 2
   read  (iostat=stat) b2              !<= unit is missing
   read  (iostat=stat, iomsg=msg, unit=2) b2

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character, intent(inout) :: iomsg

    character(3) :: temp
    read (unit, iostat=iostat, iomsg=iomsg ) temp

    call dtv%setC(temp)

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character, intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

end subroutine
