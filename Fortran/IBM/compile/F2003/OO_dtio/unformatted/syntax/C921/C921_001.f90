! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5 Data transfer statements
!*                               C921: ADVANCE= appears only in formatted squential
!*                                - ADVANCE= in formatted direct, stream
!*                                - ADVANCE= in unformatted
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
      integer(4) :: c
   end type
end module

program C921_001
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
      import base
         class (base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
      import base
         class (base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
      use m1
         class (base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(formatted)
      subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
      use m1
         class (base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), allocatable :: b1
   character(10) :: internalFile(10)
   open (1, file="C921_001.1data", form="unformatted", access="sequential" )
   open (2, file="C921_001.2data", form="formatted", access="direct", recl = 5 )
   open (3, file="C921_001.3data", form="formatted", access="stream" )
   allocate ( b1, source = base(5) )

   write (1, advance="no")                b1    !<- advance= specifier in unformatted
   write (2,"(DT)", rec=1, advance="no")  b1    !<- advance= specifier in formatted direct
   write (3,*, advance="yes", pos=1)      b1    !<- advance= specifier without explicit format spec
   write (internalFile,*, advance="yes")  b1    !<- advance= specifier with internal file

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) :: temp
    read (unit, iostat=iostat, iomsg=iomsg) temp
    dtv%c = temp

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg) dtv%c

end subroutine


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) :: temp

    read (unit, *, iostat=iostat, iomsg=iomsg) temp
    dtv%c = temp

end subroutine


subroutine formattedWrite (dtv, unit, iotype, v_list,iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
    write (unit, *, iostat=iostat, iomsg=iomsg ) " "     !<- insert space between records

end subroutine