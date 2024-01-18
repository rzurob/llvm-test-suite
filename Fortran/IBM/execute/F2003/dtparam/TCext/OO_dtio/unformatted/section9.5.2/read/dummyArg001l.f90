! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-14 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an scalar dummy argument
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

   subroutine myRead(unit, stat, msg, a, b )
      class(base(*)), intent(inout) :: a ! tcx: (*)
      class(base(*)), intent(inout), optional :: b ! tcx: (*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      if (.not. present(b) ) then
         read(unit, iostat=stat, iomsg=msg) a
      else
      	 read(unit, iostat=stat, iomsg=msg) a,b
      end if
   end subroutine

end module

program dummyArg001l
   use m1

   ! declaration of variables
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)
   type(base(:)), allocatable  :: b3 ! tcx: (:)
   type(base(:)), pointer      :: b4 ! tcx: (:)
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( b1, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b2, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b3, source = b1 )
   allocate ( b4, source = b2 )


   open (unit = 1, file ='dummyArg001l.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg)           'ibm'
   write (1, iostat=stat, iomsg=msg)           'IBMFTN'
   write (1, iostat=stat, iomsg=msg)           'ftn'
   write (1, iostat=stat, iomsg=msg)           'ibmftn'

   rewind 1

   call myRead (1, stat, msg, b1 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )             error stop 101_4
      if ( b1%c /= "ibm" )                                error stop 2_4
      msg = ''

   call myRead (1, stat, msg, b1, b2 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )             error stop 3_4
      if ( ( b1%c /= "IBM" ) .or. ( b2%c /= "FTN" ) )     error stop 4_4
      msg = ''

   call myRead (1, stat, msg, b3 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )             error stop 5_4
      if ( b3%c /= "ftn" )                                error stop 6_4
      msg = ''

   call myRead (1, stat, msg, b3, b4 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )             error stop 7_4
      if ( ( b3%c /= "ibm" ) .or. ( b4%c /= "ftn" ) )     error stop 8_4
      msg = ''

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(3) :: temp
   read (unit, iostat=iostat ) temp

   dtv%c = temp

   iomsg = 'dtio'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 10 changes
