! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : structureComp001ll
!*
!*  DATE                       : 2007-09-19 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be structure component
!*                               Sequential Access
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

   type container (lc) ! lc=3
      integer, len :: lc
      class(base(lc)), pointer     :: b1 ! tcx: (:)
      class(base(lc)), allocatable :: b2 ! tcx: (:)
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function
end module


program structureComp001ll
   use m1

   interface read(unformatted)
      subroutine readUnformattedContainer (dtv, unit, iostat, iomsg)
         import container
         class(container(*)), intent(inout) :: dtv ! tcx: (*)
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

   allocate (container(3):: b11 ) ! tcx: container(3)
   allocate (container(3):: b12 ) ! tcx: container(3)

   allocate ( b11%b1, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b11%b2, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b12%b1, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b12%b2, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b13%b1, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b13%b2, source = base(3)('xxx') )    ! tcx: (3)

   open (unit = 1, file ='structureComp001ll.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             'abcdef'
   write (1, iostat=stat, iomsg=msg )             'ghijkl'
   write (1, iostat=stat, iomsg=msg )             'mnopqr'

   rewind 1

   read (1, iostat=stat, iomsg=msg )             b11        !<- write 'abcdef' to file
      if ( ( stat /= 0 ) .or. ( msg /= 'containerdtio' ) )    error stop 101_4
      msg = ''
   read (1, iostat=stat, iomsg=msg )             b12        !<- write 'ghijkl' to file
      if ( ( stat /= 0 ) .or. ( msg /= 'containerdtio' ) )    error stop 2_4
      msg = ''
   read (1, iostat=stat, iomsg=msg )             b13        !<- write 'mnopqr' to file
      if ( ( stat /= 0 ) .or. ( msg /= 'containerdtio' ) )    error stop 3_4
      msg = ''

   ! check if the values are set correctly

   if ( ( b11%b1%c /= 'abc' ) .or. ( b11%b2%c /= 'def' ) )           error stop 4_4
   if ( ( b12%b1%c /= 'ghi' ) .or. ( b12%b2%c /= 'jkl' ) )           error stop 5_4
   if ( ( b13%b1%c /= 'mno' ) .or. ( b13%b2%c /= 'pqr' ) )           error stop 6_4

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

    if ( ( iostat /= 0 ) .or. ( iomsg /= 'basedtio' ) )    error stop 7_4

    iomsg = 'containerdtio'

end subroutine

subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    iomsg = 'basedtio'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 11 changes
! type: container - added parameters (lc) to invoke with (3) / declare with (*) - 5 changes
