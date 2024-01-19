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
!*                               - Try output item to be a function return (type bound,
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
   end type

   type, extends(base) :: child
      character(lbase_1) :: cc = ''
   end type

   contains

   function returnMyself (dtv)
      class(base(*)), intent(in) :: dtv ! tcx: (*)
      class(base(:)), allocatable :: returnMyself ! tcx: (:)

      allocate( returnMyself, source = dtv )

   end function

end module

program funcRetrn003l
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
      function foo()
      import base
         class(base(:)), allocatable :: foo ! tcx: (:)
       end function
   end interface

   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(3)   :: c1
   character(6)   :: c2, c3, c4, c5

   class(base(:)), allocatable  :: b1 ! tcx: (:)
   class(base(:)), pointer      :: b2 ! tcx: (:)
   class(child(:)), allocatable :: b3 ! tcx: (:)

   ! allocation of variables

   allocate(b1, source = base(3) ('abc')      ) ! tcx: (3)
   allocate(b2, source = child(3)('def','ghi')) ! tcx: (3)
   allocate(b3, source = child(3)('jkl','mno')) ! tcx: (3)

   open (unit = 1, file ='funcRetrn003l.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )     b1%returnMyself()
   write (1, iostat=stat, iomsg=msg )     b2%returnMyself()
   write (1, iostat=stat, iomsg=msg )     b3%returnMyself()
   write (1, iostat=stat, iomsg=msg )     bar()
   write (1, iostat=stat, iomsg=msg )     foo()

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4
   read (1, iostat=stat, iomsg=msg )              c5

   ! check if the values are set correctly

   if ( c1 /= 'abc' )                 error stop 101_4
   if ( c2 /= 'defghi' )              error stop 2_4
   if ( c3 /= 'jklmno' )              error stop 3_4
   if ( c4 /= 'pqrstu' )              error stop 4_4
   if ( c5 /= 'vwxxyz' )              error stop 5_4

   ! close the file appropriately

   close ( 1, status ='delete' )

contains
   function bar()
      class(base(:)), allocatable :: bar ! tcx: (:)
      allocate(bar, source = child(3)('pqr','stu') ) ! tcx: (3)
   end function
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

function foo()
use m1
   class(base(:)), allocatable :: foo ! tcx: (:)
   allocate(foo, source =  child(3)('vwx','xyz') ) ! tcx: (3)
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 11 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 6 changes
