! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : structureComp003lll
!*
!*  DATE                       : 2007-09-19 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be structure component, try parent component
!*                               Direct Access
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
   end type

   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: cc = ''
   end type

   type container (lcontainer_1) ! lcontainer_1=3
      integer, len :: lcontainer_1
      type(child(lcontainer_1,lcontainer_1)) :: b1 ! tcx: (lcontainer_1,lcontainer_1)
      type(child(lcontainer_1,lcontainer_1)) :: b2 ! tcx: (lcontainer_1,lcontainer_1)
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function
end module


program structureComp003lll
   use m1

   interface read(unformatted)

      subroutine readUnformattedContainer (dtv, unit, iostat, iomsg)
         import container
         class(container(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

      subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

   end interface

   ! declaration of variables
   class(container(:)), allocatable  :: b11 ! tcx: (:)
   class(container(:)), pointer      :: b12 ! tcx: (:)
   type (container(3))               :: b13 ! tcx: (3)
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( b11, source = container(3)( b2=child(3,3)('xxx','xxx'), b1=child(3,3)('xxx','xxx') ) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)
   allocate ( b12, source = container(3)( b2=child(3,3)('xxx','xxx'), b1=child(3,3)('xxx','xxx') ) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)
   b13 = container(3)( b2=child(3,3)('xxx','xxx'), b1=child(3,3)('xxx','xxx') ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3)

   open (unit = 1, file ='structureComp003lll.data', form='unformatted', access='direct', recl= 20)

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg, rec = 1 )             'abcdefjjjjjj'
   write (1, iostat=stat, iomsg=msg, rec = 19 )            'ghijkl'
   write (1, iostat=stat, iomsg=msg, rec = 13 )            'mnojjj'

   read (1, iostat=stat, iomsg=msg, rec = 1 )             b11%b1%base, b11%b2%base
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                     error stop 101_4
      if ( ( b11%b1%c /= 'abc' ) .or. ( b11%b1%cc /= 'xxx' )              .or. &
           ( b11%b2%c /= 'def' ) .or. ( b11%b2%cc /= 'xxx' ))             error stop 2_4
      msg = ''

   read (1, iostat=stat, iomsg=msg, rec=19 )             b12%b1
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                     error stop 3_4
      if ( ( b12%b1%c /= 'ghi' ) .or. ( b12%b1%cc /= 'jkl' )              .or. &
           ( b12%b2%c /= 'xxx' ) .or. ( b12%b2%cc /= 'xxx' ))             error stop 4_4
      msg = ''

   read (1, iostat=stat, iomsg=msg, rec = 13)             b13%b2%base
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                     error stop 5_4
      if ( ( b13%b1%c /= 'xxx' ) .or. ( b13%b1%cc /= 'xxx' )              .or. &
           ( b13%b2%c /= 'mno' ) .or. ( b13%b2%cc /= 'xxx' ))             error stop 6_4
      msg = ''

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformattedContainer (dtv, unit, iostat, iomsg)
use m1
    class(container(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    interface read(unformatted)
        subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
            import base
            class(base(*)), intent(inout) :: dtv ! tcx: (*)
            integer,  intent(in) :: unit
            integer,  intent(out) :: iostat
            character(*),  intent(inout) :: iomsg
        end subroutine
    end interface

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%b1, dtv%b2

    if ( (iostat /= 0 ) .or. ( iomsg /= 'basedtio' ))       error stop 7_4

    iomsg = 'containerdtio'

end subroutine

subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
use m1
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   if ( iostat /= 0 ) error stop 8_4

   select type (dtv)
      type is (child(*,*)) ! tcx: (*,*)
         read (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
   end select

   iomsg = 'basedtio'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 9 changes
! type: container - added parameters (lcontainer_1) to invoke with (3) / declare with (*) - 8 changes
