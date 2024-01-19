! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-14 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to read associate name (associated with an array)
!*                               Sequential Access
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


program associate001al
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
   class(base(:)), pointer     :: b1(:), b3(:,:) ! tcx: (:)
   class(base(:)), allocatable :: b2(:,:) ! tcx: (:)
   type(base(3))               :: b4(4) ! tcx: (3)
   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate ( b1(4), source = (/ base(3)('xxx'), base(3)('xxx'), base(3)('xxx'),base(3)('xxx') /)  ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b2(2,2), source = reshape (source=(/base(3)('xxx'),base(3)('xxx'),base(3)('xxx'),base(3)('xxx') /), shape=(/2,2/)) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b3(2,2), source = reshape (source=(/base(3)('xxx'),base(3)('xxx'),base(3)('xxx'),base(3)('xxx') /), shape=(/2,2/)) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   b4 = (/ base(3)('xxx'), base(3)('xxx'), base(3)('xxx'), base(3)('xxx') /)   ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   open (unit = 1, file ='associate001al.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )       'jklghidefabc'
   write (1, iostat=stat, iomsg=msg )       'ghijkl'
   write (1, iostat=stat, iomsg=msg )       'uvwxyz'
   write (1, iostat=stat, iomsg=msg )       'UVWXYZ'

   rewind 1

   associate ( b11 => b1(4:1:-1), b12 => b2(1,1:2), b13 => b3, b14 => b4(2:4) )

      read (1, iostat=stat, iomsg=msg )   b11         !<- shall read 'jklghidefabc' from file
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 101_4
      msg = ''
      read (1, iostat=stat, iomsg=msg )   b12(1:2)    !<- shall read 'ghijkl' from file
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 2_4
      msg = ''
      read (1, iostat=stat, iomsg=msg )   b13(1,1:2)  !<- shall read 'uvwxyz'
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 3_4
      msg = ''
      read (1, iostat=stat, iomsg=msg )   b14(1:3:2)  !<- shall read 'UVWXYZ'
      if ( (stat /= 0) .or. (msg /= 'dtio') )                           error stop 4_4
      msg = ''

      ! check if the values are set correctly for the associate names

      if ( ( b11(1)%c /= 'jkl' ) .or. ( b11(2)%c /= 'ghi' ) .or. ( b11(3)%c /= 'def' ) .or. ( b11(4)%c /= 'abc' ) )            error stop 5_4
      if ( ( b12(1)%c /= 'ghi' ) .or. ( b12(2)%c /= 'jkl' ) )                                                                  error stop 6_4
      if ( ( b13(1,1)%c /= 'uvw' ) .or. ( b13(2,1)%c /= 'xxx' ) .or. ( b13(1,2)%c /= 'xyz' ) .or. ( b13(2,2)%c /= 'xxx' ) )    error stop 7_4
      if ( ( b14(1)%c /= 'UVW' ) .or. ( b14(2)%c /= 'xxx' ) .or. ( b14(3)%c /= 'XYZ' ) )                                       error stop 8_4

   end associate

   ! check if the values are set correctly

   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) .or. ( b1(4)%c /= 'jkl' ) )                   error stop 9_4
   if ( ( b2(1,1)%c /= 'ghi' ) .or. ( b2(2,1)%c /= 'xxx' ) .or. ( b2(1,2)%c /= 'jkl' ) .or. ( b2(2,2)%c /= 'xxx' ) )           error stop 10_4
   if ( ( b3(1,1)%c /= 'uvw' ) .or. ( b3(2,1)%c /= 'xxx' ) .or. ( b3(1,2)%c /= 'xyz' ) .or. ( b3(2,2)%c /= 'xxx' ) )           error stop 11_4
   if ( ( b4(1)%c /= 'xxx' ) .or. ( b4(2)%c /= 'UVW' ) .or. ( b4(3)%c /= 'xxx' ) .or. ( b4(4)%c /= 'XYZ' ) )                   error stop 12_4

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
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 23 changes
