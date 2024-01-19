! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-09 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5 Data Transfer Statement
!*                               C909: no specifier shall appear more than once in READ or WRITE
!*                               - multiple specifier ( unit, iomsg, iostat, end )
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c
      contains
         procedure, pass :: getC
         procedure, pass :: setC
   end type
contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function

   subroutine setC (a, char)
      class(base(*)), intent(inout) :: a ! tcx: (*)
      character(3), intent(in) :: char
      a%c = char
   end subroutine
end module


program specifier001l
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)),  intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout):: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat1, stat2
   character(100) :: msg1, msg2
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer :: b2 ! tcx: (:)

   allocate(b1, source = base(3)("iBm") )  ! tcx: (3)

   allocate(base(3)::b2) ! tcx: base(3)

   open (1, file="specifier001l.data", form="unformatted", access="sequential" )

   write(1, unit=1, iostat=stat1, iomsg=msg1)  b1     !<= specifies multiple "unit" specifier implicitly
   write(unit=1, unit=2)                       b1     !<= specifies multiple "unit" specifier
   write(unit=1, iomsg=msg1, iomsg=msg2 )      b1     !<= specifies multiple "iomsg" specifier
   write(unit=1, iostat=stat1, iostat=stat2 )  b1     !<= specifies multiple "iostat" specifier

   rewind 1

   read(1, unit=1 )                            b2     !<= specifies multiple "unit" specifier
   read(1, iomsg=msg1, iomsg=msg2)             b2     !<= specifies multiple "iomsg" specifier
   read(1, iostat=stat1, iostat=stat2)         b2     !<= specifies multiple "iostat" specifier
   read(1, end=100, end=200)                   b2     !<= specifies multiple "end" specifier

   ! close the files appropriately

   close (1, status='delete')


end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: temp
    read (unit, iostat=iostat, iomsg=iomsg ) temp

    call dtv%setC(temp)

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
end subroutine


subroutine readFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: temp

    read (unit, *, iostat=iostat, iomsg=iomsg) temp
    call dtv%setC(temp)

end subroutine


subroutine writeFormatted (dtv, unit, iotype, v_list,iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, *, iostat=iostat, iomsg=iomsg) dtv%getC()
    write (unit, *, iostat=iostat, iomsg=iomsg ) " "     !<- insert space between records

end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 11 changes
