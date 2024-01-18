! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : array001l
!*
!*  DATE                       : 2007-09-13 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an allocatable array with array section
!*                                 - vector subscripts, elements
!*                               Sequential Access
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


program array001l
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

   ! declaration of variables
   class(base(:)), allocatable     :: b1(:) ! tcx: (:)
   class(base(:)), pointer         :: b2(:,:) ! tcx: (:)
   type(base(3)) :: b3(4) ! tcx: (3)
   type(base(:)),  pointer :: b4(:,:) ! tcx: (:)
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( b1(4), source = (/ base(3)('xxx'), base(3)('xxx'), base(3)('xxx'), base(3)('xxx') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b2(2,2), source = reshape (source = (/ base(3)('XXX'), base(3)('XXX'), base(3)('XXX'), base(3)('XXX')  /), shape=(/2,2/) ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   b3 = (/ base(3)('xxx'), base(3)('xxx'), base(3)('xxx'), base(3)('xxx') /) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b4(2,2), source = reshape (source = (/ base(3)('XXX'), base(3)('XXX'), base(3)('XXX'), base(3)('XXX')  /), shape=(/2,2/) ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   open (unit = 1, file ='array001l.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )              'ghidefjklabc'
   write (1, iostat=stat, iomsg=msg )              'ABCDEF'
   write (1, iostat=stat, iomsg=msg )              'stuff'
   write (1, iostat=stat, iomsg=msg )              'MNOSTU'

   rewind 1

   read (1, iostat=stat, iomsg=msg )             b1((/3,2,4,1/))    !<- read "ghidefjklabc" (try array sectino with vector subscript)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 101_4
      msg = ''
   read (1, iostat=stat, iomsg=msg )             b2(1:2:1, 1:2:2)   !<- read "ABCDEF" (try array section with subscript triplet)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 2_4
      msg = ''
   read (1, iostat=stat, iomsg=msg )             b3(3)              !<- read "stu" (try array element)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 3_4
      msg = ''
   read (1, iostat=stat, iomsg=msg )             b4(1, 1:2)         !<- read "MNOSTU" (try array section with subscript triplet)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 4_4
      msg = ''

   ! check if the values are set correctly

   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) .or. ( b1(4)%c /= 'jkl' ) )            error stop 5_4
   if ( ( b2(1,1)%c /= 'ABC' ) .or. ( b2(2,1)%c /= 'DEF' ) .or. ( b2(1,2)%c /= 'XXX' ) .or. ( b2(2,2)%c /= 'XXX' ) )    error stop 6_4
   if ( ( b3(1)%c /= 'xxx' ) .or. ( b3(2)%c /= 'xxx' ) .or. ( b3(3)%c /= 'stu' ) .or. ( b3(4)%c /= 'xxx' ) )            error stop 7_4
   if ( ( b4(1,1)%c /= 'MNO' ) .or. ( b4(2,1)%c /= 'XXX' ) .or. ( b4(1,2)%c /= 'STU' ) .or. ( b4(2,2)%c /= 'XXX' ) )    error stop 8_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(3) :: temp

   read (unit, iostat=iostat ) temp

   call dtv%setC(temp)

   iomsg = 'dtio'

end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 24 changes
