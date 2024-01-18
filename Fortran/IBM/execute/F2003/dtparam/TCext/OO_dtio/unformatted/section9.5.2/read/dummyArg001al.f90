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
!*                               - Try input item to be an scalar dummy argument of pointer/allocatable
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
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function

   subroutine myRead(unit, stat, msg, a, b )
      class(base(:)), intent(inout), allocatable :: a ! tcx: (*)
      class(base(:)), intent(inout), optional, pointer :: b ! tcx: (*)
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

program dummyArg001al
   use m1

   ! declaration of variables
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)

   integer :: stat
   character(200) :: msg
   character(4)  :: c1
   character(8)  :: c2

   ! allocation of variables
   allocate ( b1, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b2, source = base(3)('xxx') ) ! tcx: (3)

   open (unit = 1, file ='dummyArg001al.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write ( 1, iostat = stat, iomsg = msg )                'ibm'
   write ( 1, iostat = stat, iomsg = msg )                'abcdef'

   rewind 1

   call myRead (1, stat, msg, b1 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 101_4
      msg = ''
      if ( b1%c /= 'ibm' )                                              error stop 2_4
   call myRead (1, stat, msg, b1, b2 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 3_4
      msg = ''
      if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) )                   error stop 4_4

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
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
