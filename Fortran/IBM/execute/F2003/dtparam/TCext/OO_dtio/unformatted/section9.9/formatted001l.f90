! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatted001l
!*
!*  DATE                       : 2007-09-28 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - FORMATTED= specifier: Try using INQUIRE stmt with FORMATTED= specifier in procedures
!*                                                     on unformatted I/O units
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

   procedure(character(8)) :: isFormatted

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


program formatted001l
   use m1
   use ISO_FORTRAN_ENV

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
   class(base(:)), allocatable :: b1, b2, b3, b4 ! tcx: (:)
   integer :: stat1
   character(200) :: msg1

   integer, allocatable :: myUnit1, myUnit2, myUnit3

   ! allocation of variables

   allocate (base(3)::b1, b2, b3, b4) ! tcx: base(3)
   allocate (myunit1, source=1)
   allocate (myunit2, source=2)
   allocate (myunit3, source=3)

   b1%c = 'ibm'
   b2%c = 'ftn'

   if ( isFormatted(myunit1) /= 'aUNKNOWN' ) error stop 101_4
   if ( isFormatted(myunit2) /= 'aUNKNOWN' ) error stop 2_4
   if ( isFormatted(myunit3) /= 'aUNKNOWN' ) error stop 3_4

   open (myunit1, file='formatted001l.1', form='unformatted')
   open (myunit2, file='formatted001l.2', access='direct', recl=3)
   open (myunit3, file='formatted001l.3', form='unformatted', access='stream' )

   if ( isFormatted(myunit1) /= 'aNO' ) error stop 4_4
   if ( isFormatted(myunit2) /= 'aNO' ) error stop 5_4
   if ( isFormatted(myunit3) /= 'aNO' ) error stop 6_4

   ! I/O operations

   write (myunit1, iostat=stat1, iomsg=msg1 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) ) error stop 7_4

   write (myunit2, iostat=stat1, iomsg=msg1, rec=3 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) ) error stop 8_4

   write (myunit3, iostat=stat1, iomsg=msg1 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) ) error stop 9_4

   rewind 1

   read  (myunit1, iostat=stat1, iomsg=msg1 ) b2
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) ) error stop 10_4

   read  (myunit2, iostat=stat1, iomsg=msg1, rec=3 ) b3
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) ) error stop 11_4

   read  (myunit3, iostat=stat1, iomsg=msg1, pos=1 ) b4
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) ) error stop 12_4

   if ( b2%c /= 'ibm' )    error stop 13_4
   if ( b3%c /= 'ibm' )    error stop 14_4
   if ( b4%c /= 'ibm' )    error stop 15_4


   if ( isFormatted(ERROR_UNIT)  /= 'aYES' ) error stop 16_4
   if ( isFormatted(INPUT_UNIT)  /= 'aYES' ) error stop 17_4
   if ( isFormatted(OUTPUT_UNIT) /= 'aYES' ) error stop 18_4


   ! close the file appropriately

   close ( myunit1, status ='delete' )
   close ( myunit2, status ='delete' )
   close ( myunit3, status ='delete' )

   !  ERROR_UNIT cannot be closed

   close ( INPUT_UNIT, status ='delete' )
   close ( OUTPUT_UNIT, status ='delete' )

   if ( isFormatted(myunit1) /= 'aUNKNOWN' ) error stop 19_4
   if ( isFormatted(myunit2) /= 'aUNKNOWN' ) error stop 20_4
   if ( isFormatted(myunit3) /= 'aUNKNOWN' ) error stop 21_4
   if ( isFormatted(ERROR_UNIT)  /= 'aYES' ) error stop 22_4
   if ( isFormatted(INPUT_UNIT)  /= 'aUNKNOWN' ) error stop 23_4
   if ( isFormatted(OUTPUT_UNIT) /= 'aUNKNOWN' ) error stop 24_4

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   if ( iostat /= 0 ) error stop 25_4

   if ( isFormatted(unit) /= 'aNO' ) error stop 26_4

   iomsg = 'dtio read'

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

   if ( iostat /= 0 ) error stop 27_4

   FLUSH (unit, iostat=iostat, iomsg=iomsg)

   if ( isFormatted(unit) /= 'aNO' ) error stop 28_4

   iomsg = 'dtio write'

end subroutine

character(8) function isFormatted(unit)
   integer, intent(in) :: unit
   integer :: stat
   character(7) :: form1
   inquire ( unit, formatted=form1, iostat=stat )

   if ( stat /= 0 ) error stop 29_4

   if ( form1 .eq. 'YES' ) then
      isFormatted = 'aYES'
   else if ( form1 .eq. 'NO' ) then
      isFormatted = 'aNO'
   else if ( form1 .eq. 'UNKNOWN' ) then
      isFormatted = 'aUNKNOWN'
   else
      isFormatted = 'error'
   end if
end function

! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
