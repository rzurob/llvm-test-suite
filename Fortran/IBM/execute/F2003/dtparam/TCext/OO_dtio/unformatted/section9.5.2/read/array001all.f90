! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : array001all
!*
!*  PROGRAMMER                 : David Forster (derived from array001a by Robert Ma)
!*  DATE                       : 2007-09-13 (original: 11/08/2004)
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
!*                               - Try input item to be polymorphic arrays with array section
!*                                 - vector subscripts, elements (with class hierarchy and abstract type)
!*                               Stream Access
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
   type, abstract :: base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
      contains
         procedure, pass :: getC => getC
         procedure, pass :: setC
   end type

   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: cc = ''
      contains
         procedure, pass :: getC => getCC
   end type

contains

   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function

   function getCC (a)
      class(child(*,*)), intent(in) :: a ! tcx: (*,*)
      character(3) :: getCC
      getCC = a%cc
   end function

   subroutine setC (a, char)
      class(base(*)), intent(inout) :: a ! tcx: (*)
      character(3), intent(in) :: char
      a%c = char
   end subroutine

end module


program array001all
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
   class(base(:)) , allocatable :: b1(:) ! tcx: (:)
   class(child(:,:)) , allocatable :: b2(:,:) ! tcx: (:,:)
   class(base(:)) , pointer     :: b3(:) ! tcx: (:)
   class(child(:,:)), pointer     :: b4(:,:) ! tcx: (:,:)
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( b1(4), source = (/ child(3,3)('xxx','XXX'), child(3,3)('xxx','XXX'), child(3,3)('xxx','XXX'), child(3,3)('xxx','XXX') /) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
   allocate ( b2(2,2), source = reshape (source = (/ child(3,3)('XXX','xxx'), child(3,3)('XXX','xxx'), child(3,3)('XXX','xxx'), child(3,3)('XXX','xxx')  /), shape=(/2,2/) ) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
   allocate ( b3(4), source = (/ child(3,3)('xxx', 'XXX'), child(3,3)('xxx', 'XXX'), child(3,3)('xxx', 'XXX'), child(3,3)('xxx','XXX') /) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
   allocate ( b4(2,2), source = reshape (source = (/ child(3,3)('xxx', 'xxx'), child(3,3)('xxx','xxx'), child(3,3)('xxx', 'xxx'), child(3,3)('xxx','xxx')  /), shape=(/2,2/) ) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)

   open (unit = 1, file ='array001all.data', form='unformatted', access='stream')

   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg, pos=31 )             'ghiGHIdefDEFjklJKLabcABC'
   write (1, iostat=stat, iomsg=msg, pos=19 )             'ABCabcDEFdef'
   write (1, iostat=stat, iomsg=msg, pos=13 )             'stuSTU'
   write (1, iostat=stat, iomsg=msg, pos=1  )             'MNOmnoSTUstu'

   read (1, iostat=stat, iomsg=msg, pos=31 )              b1((/3,2,4,1/))
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 101_4
      msg = ''
   read (1, iostat=stat, iomsg=msg, pos=19 )              b2(1:2, 1)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 2_4
      msg = ''
   read (1, iostat=stat, iomsg=msg, pos=13 )              b3(3)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 3_4
      msg = ''
   read (1, iostat=stat, iomsg=msg, pos=1 )              b4(1:2:2, 1:2:1)
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 4_4
      msg = ''

   ! check if the values are set correctly
   
   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%getC() /= 'ABC' ) .or.            &       !<- getC() invokes getcc()
        ( b1(2)%c /= 'def' ) .or. ( b1(2)%getC() /= 'DEF' ) .or.            &
        ( b1(3)%c /= 'ghi' ) .or. ( b1(3)%getC() /= 'GHI' ) .or.            &
        ( b1(4)%c /= 'jkl' ) .or. ( b1(4)%getC() /= 'JKL' ) )               error stop 5_4

   if ( ( b2(1,1)%c /= 'ABC' ) .or. ( b2(1,1)%getC() /= 'abc' ) .or.        &
        ( b2(2,1)%c /= 'DEF' ) .or. ( b2(2,1)%getC() /= 'def' ) .or.        &
        ( b2(1,2)%c /= 'XXX' ) .or. ( b2(1,2)%getC() /= 'xxx' ) .or.        &
        ( b2(2,2)%c /= 'XXX' ) .or. ( b2(2,2)%getC() /= 'xxx' ) )           error stop 6_4

   if ( ( b3(1)%c /= 'xxx' ) .or. ( b3(1)%getC() /= 'XXX' ) .or.            &
        ( b3(2)%c /= 'xxx' ) .or. ( b3(2)%getC() /= 'XXX' ) .or.            &
        ( b3(3)%c /= 'stu' ) .or. ( b3(3)%getC() /= 'STU' ) .or.            &
        ( b3(4)%c /= 'xxx' ) .or. ( b3(4)%getC() /= 'XXX' ) )               error stop 7_4

   if ( ( b4(1,1)%c /= 'MNO' ) .or. ( b4(1,1)%getC() /= 'mno' ) .or.        &
        ( b4(2,1)%c /= 'xxx' ) .or. ( b4(2,1)%getC() /= 'xxx' ) .or.        &
        ( b4(1,2)%c /= 'STU' ) .or. ( b4(1,2)%getC() /= 'stu' ) .or.        &
        ( b4(2,2)%c /= 'xxx' ) .or. ( b4(2,2)%getC() /= 'xxx' ) )           error stop 8_4

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

   if ( iostat /= 0 ) error stop 9_4

   call dtv%setC(temp)

   select type (dtv)
      type is (child(*,*)) ! tcx: (*,*)
         read (unit, iostat=iostat ) dtv%cc
   end select

   iomsg = 'dtio'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 6 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 20 changes
