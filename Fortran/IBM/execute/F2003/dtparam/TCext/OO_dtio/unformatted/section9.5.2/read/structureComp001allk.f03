! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-19 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an structure component (without container interface)
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
   end type

   type container (kcontainer_1,lcontainer_1) ! kcontainer_1,lcontainer_1=4,3
      integer, kind :: kcontainer_1
      integer, len :: lcontainer_1
      type(base(lcontainer_1)) :: b1 ! tcx: (lcontainer_1)
      integer(kcontainer_1)    :: i
      type(base(lcontainer_1)) :: b2 ! tcx: (lcontainer_1)
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function
end module

program structureComp001allk
   use m1

    interface read(unformatted)
        subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
            import base
            class(base(*)), intent(inout) :: dtv ! tcx: (*)
            integer,  intent(in) :: unit
            integer,  intent(out) :: iostat
            character(*),  intent(inout) :: iomsg
        end subroutine
    end interface

   ! declaration of variables
   type(container(4,:)), allocatable  :: b11 ! tcx: (4,:)
   type(container(4,:)), pointer      :: b12 ! tcx: (4,:)
   type(container(4,3))               :: b13 ! tcx: (4,3)
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( b11, source = container(4,3)( b2=base(3)('xxx'), i=0, b1=base(3)('xxx') ) ) ! tcx: (3) ! tcx: (3) ! tcx: (4,3)
   allocate ( b12, source = container(4,3)( b2=base(3)('xxx'), i=0, b1=base(3)('xxx') ) ) ! tcx: (3) ! tcx: (3) ! tcx: (4,3)
   b13 = container(4,3)( b2=base(3)('xxx'), i=0, b1=base(3)('xxx') ) ! tcx: (3) ! tcx: (3) ! tcx: (4,3)

   open (unit = 1, file ='structureComp001allk.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             'abc',1,'def'
   write (1, iostat=stat, iomsg=msg )             'ghi',2,'jkl'
   write (1, iostat=stat, iomsg=msg )             'mno',3,'pqr'

   rewind 1

   read (1, iostat=stat, iomsg=msg )             b11        !<- write 'abcdef' to file
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )    error stop 101_4
      msg = ''
   read (1, iostat=stat, iomsg=msg )             b12%b1      !<- write 'ghijkl' to file
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )    error stop 2_4
      msg = ''
   read (1, iostat=stat, iomsg=msg )             b13        !<- write 'mnopqr' to file
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )    error stop 3_4
      msg = ''

   if ( ( b11%b1%c /= 'abc' ) .or. ( b11%b2%c /= 'def' ) .or. (b11%i /= 1) )           error stop 4_4
   if ( ( b12%b1%c /= 'ghi' ) .or. ( b12%b2%c /= 'xxx' ) .or. (b12%i /= 0) )           error stop 5_4
   if ( ( b13%b1%c /= 'mno' ) .or. ( b13%b2%c /= 'pqr' ) .or. (b13%i /= 3) )           error stop 6_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

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
! type: container - added parameters (kcontainer_1,lcontainer_1) to invoke with (4,3) / declare with (4,*) - 6 changes
