! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : associate001l
!*
!*  DATE                       : 2007-09-14 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to read associate name
!*                               Direct Access
!*
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
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function

   subroutine setC (a, char)
      class(base(*)), intent(inout) :: a ! tcx: (*)
      character(3), intent(in) :: char
      a%c = char
   end subroutine
end module


program associate001l
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
   class(base(:)), pointer :: b1, b2 ! tcx: (:)
   class(base(:)), allocatable :: b3 ! tcx: (:)
   type(base(3)) :: b4 ! tcx: (3)
   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate ( b1, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b2, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b3, source = base(3)('xxx') ) ! tcx: (3)
   b4 = base(3)('xxx') ! tcx: (3)

   open (unit = 1, file ='associate001l.data', form='unformatted', access='direct', recl=3)

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg, rec = 3 )       'abc'
   write (1, iostat=stat, iomsg=msg, rec = 1 )       'def'
   write (1, iostat=stat, iomsg=msg, rec = 2 )       'ghi'
   write (1, iostat=stat, iomsg=msg, rec = 4 )       'jkl'

   associate ( b11 => b1, b12 => b2, b13 => b3, b14 => b4 )
      read (1, iostat=stat, iomsg=msg, rec = 3 )   b11         !<- shall read 'abc' to file
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 101_4
      msg = ''
      read (1, iostat=stat, iomsg=msg, rec = 1 )   b12         !<- shall read 'def' to file
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 2_4
      msg = ''
      read (1, iostat=stat, iomsg=msg, rec = 2 )   b13         !<- shall read 'ghi' to file
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 3_4
      msg = ''
      read (1, iostat=stat, iomsg=msg, rec = 4 )   b14         !<- shall read 'jkl' to file
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 4_4
      msg = ''

      if ( b11%c /= 'abc' )        error stop 5_4
      if ( b12%c /= 'def' )        error stop 6_4
      if ( b13%c /= 'ghi' )        error stop 7_4
      if ( b14%c /= 'jkl' )        error stop 8_4

   end associate

   ! check if the values are set correctly

   if ( b1%c /= 'abc' )        error stop 9_4
   if ( b2%c /= 'def' )        error stop 10_4
   if ( b3%c /= 'ghi' )        error stop 11_4
   if ( b4%c /= 'jkl' )        error stop 12_4

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

   call dtv%setC(temp)

   iomsg = 'dtio'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 11 changes
