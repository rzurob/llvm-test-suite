! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar002l
!*
!*  PROGRAMMER                 : David Forster (derived from scalar002 by Robert Ma)
!*  DATE                       : 2007-09-18 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be scalar object with array component
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
      character(lbase_1), allocatable :: c(:)
   end type
end module


program scalar002l
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
   class(base(:)), allocatable     :: b1 ! tcx: (:)
   class(base(:)), pointer         :: b2 ! tcx: (:)
   type(base(:)), allocatable :: b3 ! tcx: (:)
   type(base(:)),  pointer :: b4 ! tcx: (:)
   
   integer :: stat
   character(200) :: msg
   
   character(9) :: c1
   character(6)  :: c2
   character(12)  :: c3
   character(3)  :: c4

   ! allocation of variables
     
   allocate ( b1, source = base(3) (null()) ) ! tcx: (3)
   allocate ( b2, source = base(3) (null()) ) ! tcx: (3)
   allocate ( b3, source = base(3) (null()) ) ! tcx: (3)
   allocate ( b4, source = base(3) (null()) ) ! tcx: (3)
   
   allocate (b1%c(3), source = (/ 'XXX', 'XXX', 'XXX' /) )
   allocate (b2%c(2), source = (/ 'XXX', 'XXX' /) )
   allocate (b3%c(4), source = (/ 'XXX', 'XXX', 'XXX', 'XXX' /) )
   allocate (b4%c(1), source = (/ 'XXX' /) )
      
   open (unit = 1, file ='scalar002l.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             'abcdefghi'
   write (1, iostat=stat, iomsg=msg )             'ABCDEF'
   write (1, iostat=stat, iomsg=msg )             'abcdefghijkl'
   write (1, iostat=stat, iomsg=msg )             'ABC'
   
   rewind 1
   
   read (1, iostat=stat, iomsg=msg )             b1
   if (( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 101_4
   read (1, iostat=stat, iomsg=msg )             b2
   if (( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   read (1, iostat=stat, iomsg=msg )             b3
   if (( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   read (1, iostat=stat, iomsg=msg )             b4
   if (( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   ! check if the values are set correctly

   if ( ( b1%c(1) /= 'abc' ) .or. ( b1%c(2) /= 'def' ) .or. ( b1%c(3) /= 'ghi' ) )     error stop 5_4
   if ( ( b2%c(1) /= 'ABC' ) .or. ( b2%c(2) /= 'DEF' ) )                               error stop 6_4
   if ( ( b3%c(1) /= 'abc' ) .or. ( b3%c(2) /= 'def' ) .or. ( b3%c(3) /= 'ghi' ) .or. ( b3%c(4) /= 'jkl' ) )     error stop 7_4
   if ( ( b4%c(1) /= 'ABC' ) )     error stop 8_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat )     ( dtv%c(i), i = 1, size(dtv%c,1) )
   iomsg = 'dtioread' 
   
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 10 changes
