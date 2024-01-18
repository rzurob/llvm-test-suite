! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be array of sequence type
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
   type :: base
      sequence
      character(3), pointer :: c
   end type
end module

program array004
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         type(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   type(base), allocatable   :: b3(:)
   type(base),  pointer      :: b4(:)
   type(base)                :: b5(5)

   integer :: stat
   character(200) :: msg

   character(6) :: c3 = 'abcdef'
   character(9) :: c4 = 'ghijklmno'
   character(15):: c5 = 'ABCDEFGHIJKLMNO'

   ! allocation of variables

   allocate ( b3(2), source = base (null()) )
   allocate ( b4(3), source = base (null()) )

   allocate ( b3(1)%c, b3(2)%c, b4(1)%c, b4(2)%c, b4(3)%c, b5(1)%c, b5(2)%c, b5(3)%c, b5(4)%c, b5(5)%c )

   open (unit = 1, file ='array004.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )            c3
   write (1, iostat=stat, iomsg=msg )            c4
   write (1, iostat=stat, iomsg=msg )            c5

   rewind 1

   read (1, iostat=stat, iomsg=msg )             b3
   if (( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   read (1, iostat=stat, iomsg=msg )             b4
   if (( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   read (1, iostat=stat, iomsg=msg )             b5
   if (( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   ! check if the values are set correctly

   if ( ( b3(1)%c /= 'abc' ) .or. ( b3(2)%c /= 'def' ) )          error stop 4_4
   if ( ( b4(1)%c /= 'ghi' ) .or. ( b4(2)%c /= 'jkl' ) .or. ( b4(3)%c /= 'mno' ) )     error stop 5_4
   if ( ( b5(1)%c /= 'ABC' ) .or. ( b5(2)%c /= 'DEF' ) .or. ( b5(3)%c /= 'GHI' ) .or. &
        ( b5(4)%c /= 'JKL' ) .or. ( b5(5)%c /= 'MNO' ) )     error stop 6_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   type(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( .not. associated( dtv%c ) ) then
      allocate( dtv%c )
   end if

   read (unit, iostat=iostat )     dtv%c
   iomsg = 'dtioread'

end subroutine
