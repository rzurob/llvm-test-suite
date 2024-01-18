 !#######################################################################
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: slash001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.7.2: Slash Editing
!*                                        Try slash editing inside DTIO
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
      character(3)  :: c1
      character(3)  :: c2
      character(3)  :: c3
   end type

   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

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

program slash001
   use m1

   ! declaration of variables

   class(base), allocatable :: f1
   type(base), pointer      :: f2(:)
   type(base) , allocatable :: f3
   class(base), pointer     :: f4(:)

   integer :: stat
   character(200) :: msg

   open ( unit = 1, file = 'slash001.1', access='stream', form='formatted' )

   ! allocation of variables

   allocate (f1, source = base('abc','def','ghi'))
   allocate (f2(2), source = (/ base('ABC','DEF','GHI'), base('JKL','MNO','PQR') /) )
   allocate (f3)
   allocate (base :: f4(2))

   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4
   write (1, *, iostat=stat, iomsg=msg)                f2
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4

   rewind 1

   read (1, *, iostat=stat, iomsg=msg)                 f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 3_4
   read (1, *, iostat=stat, iomsg=msg)                 f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )    error stop 4_4

   ! check if values are read correctly

   if ( (  f3%c1 /= 'abc' ) .or. (  f3%c2 /= 'def' ) .or. (  f3%c3 /= 'ghi' ) )   error stop 5_4
   if ( (  f4(1)%c1 /= 'ABC' ) .or. (  f4(1)%c2 /= 'DEF' ) .or. (  f4(1)%c3 /= 'GHI' ) .or. &
        (  f4(2)%c1 /= 'JKL' ) .or. (  f4(2)%c2 /= 'MNO' ) .or. (  f4(2)%c3 /= 'PQR' ) )   error stop 6_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format ( A3,/,1X,A3 )
   read (unit, 10, iostat=iostat )              dtv%c1, dtv%c2
   if (iostat /= 0 )   error stop 9_4
   read (unit, "(1X,A3)", iostat = iostat )     dtv%c3

   iomsg = 'dtioread'

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format ( A3,/,1X,A3 )
   write (unit, 10, iostat=iostat )              dtv%c1, dtv%c2
   if (iostat /= 0 )   error stop 9_4
   write (unit, "(1X,A3)", iostat = iostat )     dtv%c3

   iomsg = 'dtiowrite'

end subroutine
