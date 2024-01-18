! OO_dtio/formatted/section10.7/slash004.f, xlftest.OO_dtio, tstdev, 1.2
! Extract Date/Time: 05/04/07 14:46:20
! Checkin Date/Time: 05/01/18 13:31:56
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.7.2: Slash Editing
!*                                        Try slash editing on internal file
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
      character(3)   :: c1
      character(3)   :: c2(2)
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

program slash004
   use m1

   ! declaration of variables

   class(base), allocatable :: f1
   type(base), pointer      :: f2(:)
   type(base) , allocatable :: f3
   class(base), pointer     :: f4(:)

   integer :: stat
   character(200) :: msg

   character(4) :: internalFile(20)

   ! allocation of variables

   allocate (f1, source = base('ABC', (/'DEF','GHI'/)))
   allocate (f2(2), source = (/ base('abc', (/'def','ghi'/)), base('jkl', (/'mno','pqr'/)) /) )
   allocate (f3)
   allocate (base :: f4(2))

   ! formatted I/O operations

   write (internalFile(3:1:-1), *, iostat=stat, iomsg=msg)         f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4

   write (internalFile(4:10), *, iostat=stat, iomsg=msg)           f2(1:2)
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4

   read (internalFile(4:9), *, iostat=stat, iomsg=msg)             f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )     error stop 3_4
   read (internalFile(3:1:-1), *, iostat=stat, iomsg=msg)          f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )     error stop 4_4

   ! check if values are read correctly

   if ( (  f3%c1 /= 'ABC' ) .or. (  f3%c2(1) /= 'DEF' )  .or. (  f3%c2(2) /= 'GHI' )  )               error stop 5_4
   if ( (  f4(1)%c1 /= 'abc' ) .or. (  f4(1)%c2(1) /= 'def' ) .or. (  f4(1)%c2(2) /= 'ghi' ) .or. &
        (  f4(2)%c1 /= 'jkl' ) .or. (  f4(2)%c2(1) /= 'mno' ) .or. (  f4(2)%c2(2) /= 'pqr' )  )       error stop 6_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

10 format (A,/,1X,A,/,1X,A,/)
   read (unit, 10, iostat=iostat )             dtv%c1, dtv%c2

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

   character(20) :: format
   format = "(A3,/,1X,A,/,1X,A,/)"
   write (unit, fmt=format, iostat=iostat )      dtv%c1, dtv%c2

   iomsg = 'dtiowrite'

end subroutine
