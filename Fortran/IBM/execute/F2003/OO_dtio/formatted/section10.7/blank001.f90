! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.7.6: BN, BZ Editing
!*                                        Inside DTIO, try BZ editors and see if the connection has
!*                                        changed blank mode temporarily with read (I and F editors)
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

   type :: base
      integer(4)   :: i1(2)
      real(4)      :: r1(2)
      complex(4)   :: c1(2)
   end type

   interface read(formatted)
      subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program blank001
   use m1

   ! declaration of variables

   class(base), allocatable  :: f1
   type(base), pointer       :: f2(:)
   type(base) , allocatable  :: f3
   class(base), pointer      :: f4(:)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'blank001.1', form='formatted', access='sequential', blank='null' )    !<- initially set blank mode to null

   ! allocation of variables


   allocate (f1,f2(2),f3,f4(2))

   write (1, *)   "!   1  2 3  4 .50   6  . 7    8 .  90  10. 1 1  12.1  3 1 4.1  5"
   write (1, *)   "   4  5 6  6 .50   4  . 3    2 .  10  20. 3 4  56.7  8 9 8.7  6 5     4 3  2 .10   2  . 3    4 .  50  60. 7 8  99.1  0 1 1.1  2"
   write (1, *)   " 7    8 9  4 .50   6  . 7    8 .  90  10. 1 1  12.1  3 1 4.1  5"
   write (1, *)   "!  1  2  3  4 .50   6  . 7    8 .  90  10. 1 1  12.1  3 1 4.1  5 !101  2  3  4 .50   6  . 7    8 .  90  10. 1 1  12.1  3 1 4.1  5"

   rewind 1

   ! formatted I/O operations

   read (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 1_4

   read (1, '(2DT)', iostat=stat, iomsg=msg)                f2
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 2_4

   read (1, '(DT)', iostat=stat, iomsg=msg)                f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 3_4

   read (1, *, iostat=stat, iomsg=msg)                f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 4_4

   write(6, "(I4,1X,I4)")       f1%i1
   write(6, "(2(f8.4,1X))")     f1%r1
   write(6, "(4(f8.4,1X))")     f1%c1

   write(6, "(I4,1X,I4)")       f2(1)%i1
   write(6, "(2(f8.4,1X))")     f2(1)%r1
   write(6, "(4(f8.4,1X))")     f2(1)%c1
   write(6, "(I4,1X,I4)")       f2(2)%i1
   write(6, "(2(f8.4,1X))")     f2(2)%r1
   write(6, "(4(f8.4,1X))")     f2(2)%c1

   write(6, "(I4,1X,I4)")       f3%i1
   write(6, "(2(f8.4,1X))")     f3%r1
   write(6, "(4(f8.4,1X))")     f3%c1

   write(6, "(I4,1X,I4)")       f4(1)%i1
   write(6, "(2(f8.4,1X))")     f4(1)%r1
   write(6, "(4(f8.4,1X))")     f4(1)%c1
   write(6, "(I4,1X,I4)")       f4(2)%i1
   write(6, "(2(f8.4,1X))")     f4(2)%r1
   write(6, "(4(f8.4,1X))")     f4(2)%c1


end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read ( unit, iostat = iostat, fmt = '(BZ,1X,I4,1X,I4 )')          dtv%i1
   if ( iostat /= 0 ) error stop 10_4
10 format (BZ,6(1X,F8.4))
   read ( unit, iostat = iostat, fmt = 10 )   dtv%r1, dtv%c1

   iomsg = 'dtioread'

end subroutine
