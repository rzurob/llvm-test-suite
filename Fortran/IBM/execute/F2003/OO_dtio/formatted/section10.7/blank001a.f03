! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.7.6: BN, BZ Editing
!*                                        Inside DTIO, try BN and BZ editors and see if the connection has
!*                                        changed blank mode temporarily with read (G editors)
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

program blank001a
   use m1

   ! declaration of variables

   class(base), allocatable  :: f1
   class(base), pointer       :: f2(:)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'blank001a.1', form='formatted', access='sequential', blank='null' )    !<- initially set blank mode to null

   ! allocation of variables

   allocate (f1,f2(2))

   write (1, *)   "!   1  2 3   0.5  4        .17          .5 90        . 1 1        .1  3        .1  5    "
   write (1, *)   "! 4    5 6   6 .50        4 . 3         .5 10       0. 3 4      0 .7  8       0.7  6    !   1  2 3   0.5  4        .17          .5 90        . 1 1        .1  3        .1  5    "

   rewind 1

   ! formatted I/O operations

   read (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 1_4

   read (1, *, iostat=stat, iomsg=msg)                f2
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 2_4


   write(6, "(I4,1X,I4)")       f1%i1
   write(6, "(2(f8.4,1X))")     f1%r1
   write(6, "(4(f8.4,1X))")     f1%c1

   write(6, "(I4,1X,I4)")       f2(1)%i1
   write(6, "(2(f8.4,1X))")     f2(1)%r1
   write(6, "(4(f8.4,1X))")     f2(1)%c1
   write(6, "(I4,1X,I4)")       f2(2)%i1
   write(6, "(2(f8.4,1X))")     f2(2)%r1
   write(6, "(4(f8.4,1X))")     f2(2)%c1

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format (BZ,1X,I4,1X,I4,6(1X,G12.4))

   select type ( dtv )
      type is (base)
         read ( unit, iostat = iostat, fmt = 10 )   dtv
   end select

   iomsg = 'dtioread'

end subroutine