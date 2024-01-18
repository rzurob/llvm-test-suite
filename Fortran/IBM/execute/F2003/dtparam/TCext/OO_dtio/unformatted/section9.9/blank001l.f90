! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : blank001l
!*
!*  DATE                       : 2007-09-19 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: BLANK= specifier: Try using INQUIRE stmt with BLANK= specifier in procedures
!*                                                     on non-opened unit and unformatted units
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
contains
   character(10) function isBlank(unit)
      integer, intent(in) :: unit
      integer :: stat
      character(9) :: blank1
      inquire ( unit, blank=blank1, iostat=stat )

      if ( stat /= 0 ) error stop 102_4

      if ( blank1 .eq. 'ZERO' ) then
         isBlank = 'aZERO'
      else if ( blank1 .eq. 'NULL' ) then
         isBlank = 'aNULL'
      else if ( blank1 .eq. 'UNDEFINED' ) then
         isBlank = 'aUNDEFINED'
      else
         isBlank = 'error'
      end if
   end function
end module

module m1
   use m
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
      contains
         procedure, pass :: getC
         procedure, pass :: setC
   end type

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


program blank001l
   use m1

   ! declaration of variables
   class(base(:)), allocatable :: b1, b2 ! tcx: (:)
   integer :: stat1
   character(200) :: msg1

   integer, pointer :: myUnitPtr
   integer, target :: i = 3

   ! allocation of variables

   allocate (base(3)::b1,b2) ! tcx: base(3)
   myUnitPtr => i

   b1%c = 'ibm'
   b2%c = 'ftn'

   if ( isBlank(myUnitPtr) /= 'aUNDEFINED' )   error stop 101_4

   open ( 3, file='blank001l.data', form='unformatted', access='direct', recl=3 )

   if ( isBlank(myUnitPtr) /= 'aUNDEFINED' )          error stop 2_4

   ! I/O operations

   write ( 3, iostat=stat1, iomsg=msg1, rec=4) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) ) error stop 3_4


   read  (myUnitPtr, iostat=stat1, iomsg=msg1, rec=4 ) b2
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )  error stop 4_4

   if ( b2%c /= 'ibm' ) error stop 5_4

   ! close the file appropriately

   close ( myUnitPtr, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, isBlank
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   if ( iostat /= 0 ) error stop 6_4

   if ( isBlank(unit) /= 'aUNDEFINED' ) error stop 7_4

   iomsg = 'dtio read'

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, isBlank
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

   if ( iostat /= 0 ) error stop 8_4

   FLUSH (unit, iostat=iostat, iomsg=iomsg)

   if ( isBlank(unit) /= 'aUNDEFINED' ) error stop 9_4

   iomsg = 'dtio write'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
