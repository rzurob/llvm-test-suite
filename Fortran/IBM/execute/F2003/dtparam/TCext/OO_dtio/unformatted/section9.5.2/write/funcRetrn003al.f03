! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be a function return
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
      procedure, pass :: returnMyself
      procedure, pass :: returnXCopiesOfMyself
   end type

   type, extends(base) :: child
      character(lbase_1) :: cc = ''
   end type

   contains

   elemental function returnMyself (dtv)
      class(base(*)), intent(in) :: dtv ! tcx: (*)
      type(base(3)) :: returnMyself ! tcx: (3)

      returnMyself = dtv

   end function

   function returnXCopiesOfMyself (dtv, x)
      class(base(*)), intent(in) :: dtv ! tcx: (*)
      integer(4), intent(in)  :: x
      class(base(:)), allocatable :: returnXCopiesOfMyself(:) ! tcx: (:)

      allocate( returnXCopiesOfMyself(x), source = (/ (dtv, j=1,x) /) )
   end function

end module

program funcRetrn003al
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
   character(6)   :: c1, c3
   character(12)   :: c2, c6
   character(18)   :: c4
   character(30)   :: c5
   class(base(:)), allocatable  :: b1(:) ! tcx: (:)
   class(base(:)), pointer      :: b2(:,:) ! tcx: (:)
   class(child(:)), allocatable :: b3(:) ! tcx: (:)

   ! allocation of variables

   allocate(b1(2), source = (/ child(3)('abc','def'), child(3)('ghi','jkl') /) ) ! tcx: (3) ! tcx: (3)
   allocate(b2(2,2), source = reshape(source=(/b1, b1/), shape=(/2,2/) ) )
   allocate(b3(4:5), source = (/ child(3)('mno','pqr'), child(3)('stu','vwx') /)) ! tcx: (3) ! tcx: (3)

   open (unit = 1, file ='funcRetrn003al.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )     b1%returnMyself()
   write (1, iostat=stat, iomsg=msg )     b2%returnMyself()
   write (1, iostat=stat, iomsg=msg )     b3%returnMyself()
   write (1, iostat=stat, iomsg=msg )     b1(1)%returnXCopiesOfMyself(3)
   write (1, iostat=stat, iomsg=msg )     b2(2,1)%returnXCopiesOfMyself(5)
   write (1, iostat=stat, iomsg=msg )     b3(5)%returnXCopiesOfMyself(2)

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4
   read (1, iostat=stat, iomsg=msg )              c5
   read (1, iostat=stat, iomsg=msg )              c6

   ! check if the values are set correctly

   if ( c1 /= 'abcghi' )                          error stop 101_4
   if ( c2 /= 'abcghiabcghi' )                    error stop 2_4
   if ( c3 /= 'mnostu' )                          error stop 3_4
   if ( c4 /= 'abcdefabcdefabcdef' )              error stop 4_4
   if ( c5 /= 'ghijklghijklghijklghijklghijkl' )  error stop 5_4
   if ( c6 /= 'stuvwxstuvwx' )                    error stop 6_4

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
         write (unit, iostat=iostat) dtv%c
      type is (child(*)) ! tcx: (*)
         write (unit, iostat=iostat) dtv%c, dtv%cc
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 6 changes
