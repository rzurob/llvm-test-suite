! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : arrayConstr002lk
!*
!*  PROGRAMMER                 : David Forster (derived from arrayConstr002 by Robert Ma)
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 (Data Transfer input/output list)
!*                               - output item is an array constructor with polymorphic variables
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
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
   end type

end module


program arrayConstr002lk
   use m1

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

   integer :: stat
   character(200) :: msg

   character(3) :: s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13
   character(15) :: u1
   integer :: i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13

   class(base(:)), allocatable  :: b1(:), b2 ! tcx: (:)
   class(child(:,4)), pointer     :: c1(:,:), c2 ! tcx: (:,4)

   type(base(3))  :: b3, b4(3) ! tcx: (3)
   type(child(3,4)) :: c3, c4(2) ! tcx: (3,4)

   ! allocation of variables

   allocate ( b1(3), source = (/ child(3,4)('abc', 101), child(3,4)('def', 102), child(3,4)('ghi', 103) /) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   allocate ( b2, source = child(3,4)('ABC', 104) ) ! tcx: (3,4)

   allocate ( c1(2,2), source = reshape ( source = (/ child(3,4)('abc', 201 ), child(3,4)('def', 202 ), child(3,4)('ghi', 203 ), child(3,4) ('jkl', 204 ) /), shape = (/2,2/) ) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   allocate ( c2, source = child(3,4)('ABC', 205 ) ) ! tcx: (3,4)

   b3 = base(3)('ABC') ! tcx: (3)
   b4 = (/ base(3)('AAA') , base(3)('BBB'), base(3)('CCC') /) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   c3 = child(3,4) ( 'DEF', 301 ) ! tcx: (3,4)
   c4 = (/ child(3,4) ( 'aaa', 302 ), child(3,4) ( 'bbb', 303 ) /) ! tcx: (3,4) ! tcx: (3,4)

   open (unit = 1, file ='arrayConstr002lk.data', form='unformatted', access='sequential')

   ! I/O operations

   write (1, iostat=stat, iomsg=msg ) (/ b1, b2 /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 101_4

   write (1, iostat=stat, iomsg=msg ) (/ b3, b4(1), b4(2:3), base(3)('DDD') /) ! tcx: (3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write (1, iostat=stat, iomsg=msg ) (/ reshape (source = c1, shape = (/4/) ), c2 /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   write (1, iostat=stat, iomsg=msg ) (/ c3, c4, child(3,4)('ccc', 304) /) ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4

   rewind 1

   read (1, iostat=stat, iomsg=msg )              s1, i1, s2, i2, s3, i3, s4, i4
   read (1, iostat=stat, iomsg=msg )              u1
   read (1, iostat=stat, iomsg=msg )              s5, i5, s6, i6, s7, i7, s8, i8, s9, i9
   read (1, iostat=stat, iomsg=msg )              s10, i10, s11, i11, s12, i12, s13, i13

   ! check if the values are set correctly

   if ( ( s1 /= 'abc' ) .or. ( i1 /= 101 ) .or. &
        ( s2 /= 'def' ) .or. ( i2 /= 102 ) .or. &
        ( s3 /= 'ghi' ) .or. ( i3 /= 103 ) .or. &
        ( s4 /= 'ABC' ) .or. ( i4 /= 104 ) )   error stop 5_4

   if ( u1 /= 'ABCAAABBBCCCDDD' )              error stop 6_4

   if ( ( s5 /= 'abc' ) .or. ( i5 /= 201 ) .or. &
        ( s6 /= 'def' ) .or. ( i6 /= 202 ) .or. &
        ( s7 /= 'ghi' ) .or. ( i7 /= 203 ) .or. &
        ( s8 /= 'jkl' ) .or. ( i8 /= 204 ) .or. &
        ( s9 /= 'ABC' ) .or. ( i9 /= 205 ) )   error stop 7_4

   if ( ( s10 /= 'DEF' ) .or. ( i10 /= 301 ) .or. &
        ( s11 /= 'aaa' ) .or. ( i11 /= 302 ) .or. &
        ( s12 /= 'bbb' ) .or. ( i12 /= 303 ) .or. &
        ( s13 /= 'ccc' ) .or. ( i13 /= 304 ) ) error stop 8_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base(*)) ! tcx: (*)
         write (unit, iostat=iostat, iomsg=iomsg ) dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         write (unit, iostat=iostat, iomsg=iomsg ) dtv%c, dtv%i
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 10 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 16 changes
