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
!*                                    - Dummy Argument being non-polymorphic array entity with unformatted i/o
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
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   interface
      subroutine mywriteext ( dtv, j )
         import base
         type(base), pointer, intent(in) :: dtv(:)
         integer, intent(in) :: j
      end subroutine
   end interface

   interface
      subroutine myreadext ( dtv, j )
         import base
         type(base), allocatable, intent(inout) :: dtv(:)
         integer, intent(in) :: j
      end subroutine
   end interface

   contains

      subroutine write (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine read (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine mywrite ( dtv, j )
         type(base), intent(in) :: dtv(3)
         integer, intent(in) :: j
         integer :: stat
         character(200) :: msg


         write ( 1, iostat = stat, iomsg = msg, rec = j ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

      end subroutine

      subroutine myread ( dtv, j )
         type(base), intent(inout) :: dtv(:)
         integer, intent(in) :: j
         integer :: stat
         character(200) :: msg

         read ( 1, iostat = stat, iomsg = msg, rec = j ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 2_4

      end subroutine

end module

program dummyArg003
   use m

   type(base)              :: b1(3) = (/ base('abc'), base('def'), base('ghi') /)
   type(base), allocatable :: b2(:)
   type(base), pointer     :: b3(:)

   open ( 1, file='dummyArg003.1', form='unformatted', access='direct', recl=20 )

   allocate ( b2(3), source = (/ base('ABC'), base('DEF'), base('GHI') /) )
   allocate ( b3(3), source = (/ base('jkl'), base('mno'), base('pqr') /) )

   call mywrite ( b1, 6 )
   call mywrite ( b2, 1 )
   call mywrite ( b3, 5 )

   call mywriteext ( b3, 10 )

   call myread  ( b1, 5 )
   call myread  ( b2, 6 )
   call myread  ( b3, 1 )

   if ( ( b1(1)%c /= 'jkl' ) .or. ( b1(2)%c /= 'mno' ) .or. ( b1(3)%c /= 'pqr' ) .or. &
        ( b2(1)%c /= 'abc' ) .or. ( b2(2)%c /= 'def' ) .or. ( b2(3)%c /= 'ghi' ) .or. &
        ( b3(1)%c /= 'ABC' ) .or. ( b3(2)%c /= 'DEF' ) .or. ( b3(3)%c /= 'GHI' ) )     error stop 3_4

   call myreadext ( b2, 10 )

   if ( ( b2(1)%c /= 'jkl' ) .or. ( b2(2)%c /= 'mno' ) .or. ( b2(3)%c /= 'pqr' ) )     error stop 4_4

   close ( 1, status ='delete')

end program

subroutine mywriteext ( dtv, j )
   use m, only: base
   type(base), pointer, intent(in) :: dtv(:)
   integer, intent(in) :: j
   integer :: stat
   character(200) :: msg

   write ( 1, iostat = stat, iomsg = msg, rec = j ) dtv
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 5_4

end subroutine

subroutine myreadext ( dtv, j )
   use m, only: base
   type(base), allocatable, intent(inout) :: dtv(:)
   integer, intent(in) :: j
   integer :: stat
   character(200) :: msg

   read ( 1, iostat = stat, iomsg = msg, rec = j ) dtv
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 6_4

end subroutine
