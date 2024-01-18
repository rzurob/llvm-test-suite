!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 04/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Dummy Argument Association
!*                                    - Dummy Argument being non-polymorphic assumed-size array entity with formatted i/o
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base
      character(3) :: c = 'xxx'
      contains
         procedure, pass :: write
         procedure, pass :: read
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   interface
      subroutine mywriteext ( dtv )
         import base
         type(base), intent(in) :: dtv(1,*)
      end subroutine
   end interface

   interface
      subroutine myreadext ( dtv )
         import base
         type(base), intent(inout) :: dtv(2,*)
      end subroutine
   end interface

   contains

      subroutine write (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine read (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine mywrite ( dtv )
         type(base), intent(in) :: dtv(1,*)
         integer :: stat
         character(200) :: msg

         write ( 1, *, iostat = stat, iomsg = msg )      dtv(1,1:4)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

      end subroutine

      subroutine myread ( dtv )
         type(base), intent(inout) :: dtv(2,*)
         integer :: stat
         character(200) :: msg

         read ( 1, "(1x,4(DT))", iostat = stat, iomsg = msg ) dtv(1:2,1:2)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 2_4

      end subroutine

end module

program dummyArg004
   use m

   type(base)              :: b1(4) = (/ base('abc'), base('def'), base('ghi'), base('jkl') /) 
   type(base), allocatable :: b2(:,:)
   type(base), allocatable :: b3(:)

   open ( 1, file='dummyArg004.1', form='formatted', access='sequential' )

   allocate ( b2(2,2), source = reshape ( source = (/ base('ABC'), base('DEF'), base('GHI'), base('JKL') /), shape = (/2,2/) ) )
   allocate ( b3(4), source = (/ base('mno'), base('pqr'), base('stu'), base('vwx') /) )

   call mywrite ( b1 )
   call mywrite ( b2 )
   call mywrite ( b3 )

   call mywriteext ( b3 )
 
   rewind 1

   call myread  ( b2 )
   call myread  ( b3 )
   call myread  ( b1 )

   if ( ( b1(1)%c /= 'mno' ) .or. ( b1(2)%c /= 'pqr' ) .or. ( b1(3)%c /= 'stu' ) .or. ( b1(4)%c /= 'vwx' ) .or. &
        ( b2(1,1)%c /= 'abc' ) .or. ( b2(2,1)%c /= 'def' ) .or. ( b2(1,2)%c /= 'ghi' ) .or. ( b2(2,2)%c /= 'jkl' ) .or. &
        ( b3(1)%c /= 'ABC' ) .or. ( b3(2)%c /= 'DEF' ) .or. ( b3(3)%c /= 'GHI' ) .or. ( b3(4)%c /= 'JKL' ) )     error stop 3_4

   call myreadext ( b2 )

   if ( ( b2(1,1)%c /= 'mno' ) .or. ( b2(2,1)%c /= 'pqr' ) .or. ( b2(1,2)%c /= 'stu' ) .or. ( b2(2,2)%c /= 'vwx' )  )     error stop 4_4
   close ( 1, status ='delete')

end program

subroutine mywriteext ( dtv )
   use m, only: base
   type(base), intent(in) :: dtv(1,*)
   integer :: stat
   character(200) :: msg

   write ( 1, *, iostat = stat, iomsg = msg ) dtv(1,1:4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 5_4

end subroutine

subroutine myreadext ( dtv )
   use m, only: base
   type(base), intent(inout) :: dtv(2,*)
   integer :: stat
   character(200) :: msg

   read ( 1, "(1x,4(DT))", iostat = stat, iomsg = msg ) dtv(1:2,1:2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 6_4

end subroutine
