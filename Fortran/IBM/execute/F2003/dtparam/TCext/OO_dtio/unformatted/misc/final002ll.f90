! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : final002ll
!*
!*  DATE                       : 2007-09-10 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Finalization process
!*                               Try DTIO inside the finalization process with class hierarchy
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
      final :: finalbase
   end type

   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: cc = ''
   contains
      final :: finalchild, finalchildrank1
   end type

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

contains

   character(3) function getC(dtv)
      class(base(*)), intent(in) :: dtv ! tcx: (*)
      getC = dtv%c
   end function

   subroutine setC(dtv, c)
      class(base(*)), intent(inout) :: dtv ! tcx: (*)
      character(3) :: c
      dtv%c = c
   end subroutine

   subroutine finalchild(dtv)
      type(child(*,*)), intent(in) :: dtv ! tcx: (*,*)
      integer :: stat
      character(200) :: msg

      !! Note: This has been patched to avoid potential output problems
      if(dtv%lchild_1 /= 3) then
         print *, 'finalize child: dtv%lbase_1=', dtv%lbase_1, 'dtv%lchild_1=', dtv%lchild_1, '(error: unexpected length)'
      else
         print *, 'finalize child ', dtv%cc
         write (unit = 3, iostat=stat, iomsg=msg)   dtv
         if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )   error stop 101_4
      end if

   end subroutine

   subroutine finalchildrank1(dtv)
      type(child(*,*)), intent(in) :: dtv(:) ! tcx: (*,*)
      integer :: stat
      character(200) :: msg

      !! Note: This has been patched to avoid potential output problems
      if(dtv%lchild_1/=3) then
         print *, 'finalize child rank=1: dtv%lbase_1=', dtv%lbase_1, 'dtv%lchild_1=', dtv%lchild_1, size(dtv), '(error: unexpected length)'
      else
         print *, 'finalize child rank 1 ', dtv%cc
         write (unit = 3, iostat=stat, iomsg=msg)   dtv
         if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )   error stop 2_4
      end if

   end subroutine

   subroutine finalbase(dtv)
      type(base(*)), intent(in) :: dtv ! tcx: (*)

      !! Note: This has been patched to avoid potential output problems
      if(dtv%lbase_1/=3) then
         print *, 'finalize base: dtv%lbase_1=', dtv%lbase_1, '(error: unexpected length)'
      else
         print *, 'finalize base ', dtv%c
      end if

   end subroutine

end module


program final002ll
   use m1

   ! declaration of variables
   class(child(:,:)), allocatable :: c1 ! tcx: (:,:)
   class(child(:,:)), pointer     :: c2 ! tcx: (:,:)
   class(child(:,:)), allocatable :: c3 ! tcx: (:,:)
   class(child(:,:)), pointer     :: c4(:) ! tcx: (:,:)

   integer :: stat
   character(200) :: msg

   open (unit = 3, file ='final002ll.3', form='unformatted', access='sequential')

   ! allocation of variables

   allocate (c1, source = child(3,3)('abc', 'ABC') )           !<- finalized ! tcx: (3,3)
   allocate (c2, source = child(3,3)('def', 'DEF') )           !<- finalized ! tcx: (3,3)
   allocate (c3, source = child(3,3)('ghi', 'GHI') )           !<- finalized ! tcx: (3,3)
   allocate (c4(3), source = (/ c1, child(3,3)('jkl', 'JKL') , c3 /) )   !<- finalize 1 child scalar ! tcx: (3,3)

   rewind 3

   ! unformatted I/O operations

   read ( 3, iostat = stat, iomsg = msg )   c4(1)
   read ( 3, iostat = stat, iomsg = msg )   c4(2)
   read ( 3, iostat = stat, iomsg = msg )   c4(3)

   backspace 3
   backspace 3

   read ( 3, iostat = stat, iomsg = msg )   c3
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 3_4
   read ( 3, iostat = stat, iomsg = msg )   c2
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 4_4
   read ( 3, iostat = stat, iomsg = msg )   c1
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 5_4

   if ( .not. check (c1,'jkl','JKL') )                error stop 6_4
   if ( .not. check (c2,'ghi','GHI') )                error stop 7_4
   if ( .not. check (c3,'def','DEF') )                error stop 8_4
   if ( ( .not. check( c4(1),'abc','ABC') ) .or. ( .not. check( c4(2),'def','DEF' ) ) .or. &
        ( .not. check( c4(3),'ghi','GHI') ) )         error stop 9_4

   rewind 3

   deallocate (c1)  !<- finalized
   deallocate (c2)  !<- finalized
   deallocate (c3)  !<- finalized
   deallocate (c4)  !<- finalized

   allocate(child(3,3):: c1, c2, c3, c4(3) ) ! tcx: child(3,3)
   rewind 3

   read ( 3, iostat = stat, iomsg = msg )   c3
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 10_4
   read ( 3, iostat = stat, iomsg = msg )   c1
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 11_4
   read ( 3, iostat = stat, iomsg = msg )   c2
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 12_4
   read ( 3, iostat = stat, iomsg = msg )   c4
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 13_4

   if ( .not. check (c1,'ghi','GHI') )                error stop 14_4
   if ( .not. check (c2,'def','DEF') )                error stop 15_4
   if ( .not. check (c3,'jkl','JKL') )                error stop 16_4

   if ( ( .not. check( c4(1),'abc','ABC') ) .or. ( .not. check( c4(2),'def','DEF' ) ) .or. &
        ( .not. check( c4(3),'ghi','GHI') ) )         error stop 17_4

   ! close the file appropriately

   close ( 3, status ='delete' )

contains

   logical function check(dtv, a, b)
      class(base(*)) :: dtv ! tcx: (*)
      character(3) :: a
      character(3), optional :: b

      select type (dtv)
         type is (base(*)) ! tcx: (*)
            check = ( dtv%c == a )
         type is (child(*,*)) ! tcx: (*,*)
            if ( present(b) ) then
               check = ( ( dtv%c == a ) .and. ( dtv%cc == b ) )
            else
               error stop 18_4
            end if
         class default
            error stop 19_4
      end select

   end function

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit,  iostat=iostat ) dtv%c

   select type (dtv)
      type is (child(*,*)) ! tcx: (*,*)
         read (unit,  iostat=iostat ) dtv%cc
   end select

   iomsg = 'dtioread'

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1,  only: base, child
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat ) dtv%c

   select type (dtv)
      type is (child(*,*)) ! tcx: (*,*)
         write (unit,  iostat=iostat ) dtv%cc
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 1 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 3 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 13 changes
