! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn004ll
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be a non-polymorphic function return (pointer, allocatable, and neither)
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

   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: cc = ''
   end type

contains

   function getNewBase(c)
      type(base(3)) :: getNewBase ! tcx: (3)
      character(3) :: c
      getNewBase = base(3)(c) ! tcx: (3)
   end function

   function getNewChild(c)
      type(child(3,3)) :: getNewChild ! tcx: (3,3)
      character(6) :: c
      getNewChild = child(3,3)(c(1:3),c(4:6)) ! tcx: (3,3)
   end function

end module

program funcRetrn004ll
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

   interface
      function getNewBasePtr(c)
         import base
         type(base(:)), pointer :: getNewBasePtr ! tcx: (:)
         character(3) :: c
      end function
   end interface

   interface
      function getNewChildAlloc(c)
         import child
         type(child(:,:)), allocatable :: getNewChildAlloc ! tcx: (:,:)
         character(6) :: c
      end function
   end interface

   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(3)   :: c1, c3
   character(6)   :: c2
   character(9)   :: c4

   open (unit = 1, file ='funcRetrn004ll.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             getNewBase('abc')
   write (1, iostat=stat, iomsg=msg )             getNewChild('defghi')
   write (1, iostat=stat, iomsg=msg )             getNewBasePtr('jkl')
   write (1, iostat=stat, iomsg=msg )             getNewChildAlloc('mnopqr'), getNewBasePtr('stu')

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4

   ! check if the values are set correctly

   if ( c1 /= 'abc' )                      error stop 101_4
   if ( c2 /= 'defghi' )                   error stop 2_4
   if ( c3 /= 'jkl' )                      error stop 3_4
   if ( c4 /= 'mnopqrstu' )                error stop 4_4

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
      type is (child(*,*)) ! tcx: (*,*)
         write (unit, iostat=iostat) dtv%c, dtv%cc
   end select
   iomsg = 'dtiowrite'
end subroutine

function getNewBasePtr(c)
   use m1
   type(base(:)), pointer :: getNewBasePtr ! tcx: (:)
   character(3) :: c
   allocate ( getNewBasePtr, source = base(3)(c) ) ! tcx: (3)
end function

function getNewChildAlloc(c)
   use m1
   type(child(:,:)), allocatable :: getNewChildAlloc ! tcx: (:,:)
   character(6) :: c
   allocate(getNewChildAlloc, source = child(3,3)(c(1:3),c(4:6)) ) ! tcx: (3,3)
 end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 8 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 6 changes
