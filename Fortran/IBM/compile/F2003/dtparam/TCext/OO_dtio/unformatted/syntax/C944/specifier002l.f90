! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : specifier002l
!*
!*  DATE                       : 2007-09-09 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.8: FLUSH statement
!*                               - Specify the same specifier more than once in the FLUSH stmt (try unit and err)
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
      character(lbase_1) :: c = ''
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


program specifier002l
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
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

   ! declaration of variables
   class(base(:)), allocatable :: b1, b2 ! tcx: (:)
   integer :: stat1
   character(200) :: msg1

   ! allocation of variables

   allocate (base(3)::b1,b2) ! tcx: base(3)

   b1%c = 'ibm'
   b2%c = ''

   ! I/O operations

   FLUSH (1, iostat = stat1, unit = 2 )   !<- flush a file that does not exist and specify unit twice

   if ( stat1 /= 0 ) error stop 101_4

   open (unit = 1, file ='specifier002l.data', form='unformatted', access='sequential')

   write (1, iostat=stat1, iomsg = msg1)    b1

   if ( ( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) )           error stop 2_4
   msg1 = ''

   FLUSH (1, iostat = stat1, err = 100, err = 200 )   !<- flush a file that exists and specify err twice

   if ( stat1 /= 0 ) error stop 3_4

   rewind 1

   read (1, iostat=stat1, iomsg = msg1)    b2

100 if ( ( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )           error stop 4_4

200 if ( b2%c /= 'ibm' ) error stop 5_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    FLUSH (unit, err = 100, err = 200 )   !<- flush inside DTIO and specify err twice

100 iomsg = 'dtio read'

200 FLUSH (unit)

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

    FLUSH (unit, unit=1 )   !<- flush inside DTIO and specify unit twice

    iomsg = 'dtio write'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
