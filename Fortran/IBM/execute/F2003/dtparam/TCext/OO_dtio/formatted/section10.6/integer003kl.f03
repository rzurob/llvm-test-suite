! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.1.1: Integer Editing
!*                                        Try different integer editing descriptor in child data transfer stmt
!*                                        First try B, O, X, and I with w == 0
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
   type fourintegers (kf1,kf2,kf3,kf4) ! kf1,kf2,kf3,kf4=1,2,4,8
      integer, kind :: kf1,kf2,kf3,kf4
      integer(kf1) :: c1 = 0
      integer(kf2) :: c2 = 0
      integer(kf3) :: c3 = 0
      integer(kf4) :: c4 = 0
   end type
end module


program integer003kl
   use m1

   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import fourintegers
         class(fourintegers(1,2,4,8)), intent(in) :: dtv ! tcx: (1,2,4,8)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import fourintegers
         class(fourintegers(1,2,4,8)), intent(inout) :: dtv ! tcx: (1,2,4,8)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   class(fourintegers(1,2,4,8)), allocatable :: f1 ! tcx: (1,2,4,8)
   class(fourintegers(1,2,4,8)), pointer     :: f2 ! tcx: (1,2,4,8)
   type(fourintegers(1,2,4,8)) , allocatable :: f3 ! tcx: (1,2,4,8)
   type(fourintegers(1,2,4,8)) , pointer     :: f4 ! tcx: (1,2,4,8)

   integer :: stat
   character(200) :: msg

   allocate (f1, source = fourintegers(1,2,4,8)(1_1,2_2,3_4,4_8))                                          !<- basic test  ! tcx: (1,2,4,8)
   allocate (f2, source = fourintegers(1,2,4,8)(-127_1,-32768_2,-2147483648_4,-9223372036854775807_8))       !<- with maximum values ! tcx: (1,2,4,8)
   allocate (f3, source = fourintegers(1,2,4,8)(-128_1,-32767_2,-2147483648_4,-9223372036854775807_8))      !<- with maximum values ! tcx: (1,2,4,8)
   allocate (f4, source = fourintegers(1,2,4,8)(-128_1,-32768_2,-2147483648_4,-9223372036854775808_8))     !<- with maximum values ! tcx: (1,2,4,8)

   open (unit = 1, file ='integer003kl.1', form='formatted', access='sequential')
   open (unit = 2, file ='integer003kl.2', form='formatted', access='stream' )

   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)             f1
   write (2, *, iostat=stat, iomsg=msg)             f2, f3, f4

   rewind 1
   rewind 2

   deallocate (f1)
   allocate   (f1)

   read  (1, "(DT)", iostat=stat, iomsg=msg)             f1
   read  (2, *, iostat=stat, iomsg=msg)             f4, f2, f3

   ! check if values are read correctly
   if ( .not. check( f1, 1_1,2_2,3_4,4_8 ) )       error stop 2_4

   if ( (.not. check( f2, -128_1,-32767_2,-2147483648_4,-9223372036854775807_8 ) ) .or.  &
        (.not. check( f3, -128_1,-32768_2,-2147483648_4,-9223372036854775808_8 ) ) .or.  &
        (.not. check( f4, -127_1,-32768_2,-2147483648_4,-9223372036854775807_8 ) ) )    error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )
   close ( 2, status ='delete' )

contains

   logical function check(dtv, a, b, c, d)
      class(fourintegers(1,2,4,8)), intent(in) :: dtv ! tcx: (1,2,4,8)
      integer(1), intent(in) :: a
      integer(2), intent(in) :: b
      integer(4), intent(in) :: c
      integer(8), intent(in) :: d

      check = ( ( dtv%c1 == a ) .and. ( dtv%c2 == b ) .and. ( dtv%c3 == c ) .and. ( dtv%c4 == d ) )

   end function

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
   class(fourintegers(1,2,4,8)), intent(inout) :: dtv ! tcx: (1,2,4,8)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(10) :: format, format1, format2, format3, access1
   integer :: stat1, stat2, stat3, stat4

   inquire ( unit, access=access1 )

10 format (1X,I1)
20 format (1X,B2)
   format = "(1X,O1)"
   format1 = "(1X,Z1)"

30 format (I4)
40 format (1X,B32)
   format2 = "(1X,O6)"
   format3 = "(1X,Z16)"


   if ( access1 == "SEQUENTIAL" ) then
      read (unit, 10, iomsg=iomsg, iostat=stat1 )          dtv%c1
      read (unit, fmt=format, iomsg=iomsg, iostat=stat2 )  dtv%c2
      read (unit, 20, iomsg=iomsg, iostat=stat3 )          dtv%c3
      read (unit, fmt=format1, iomsg=iomsg, iostat=stat4 ) dtv%c4
   else if ( access1 == "STREAM" ) then
      read (unit, 30, iomsg=iomsg, iostat=stat1 )          dtv%c1
      read (unit, fmt=format2, iomsg=iomsg, iostat=stat2 ) dtv%c2
      read (unit, 40, iomsg=iomsg, iostat=stat3 )          dtv%c3
      read (unit, fmt=format3, iomsg=iomsg, iostat=stat4 ) dtv%c4
   else
      error stop 5_4
   end if

   if ( ( stat1 /= 0 ) .or. ( stat2 /= 0 ) .or. ( stat3 /= 0 ) .or. ( stat4 /= 0 ) )        error stop 6_4

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
   class(fourintegers(1,2,4,8)), intent(in) :: dtv ! tcx: (1,2,4,8)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(8) :: format, format1
   integer :: stat1, stat2, stat3, stat4

10 format (I0)           !<- when w is 0, it uses the least amount of characters
20 format (1X,B0)
   format = "(1X,O0)"
   format1 = "(1X,Z0)"

   write (unit, 10, iomsg=iomsg, iostat=stat1 )          dtv%c1
   write (unit, fmt=format, iomsg=iomsg, iostat=stat2 )  dtv%c2
   write (unit, 20, iomsg=iomsg, iostat=stat3 )          dtv%c3
   write (unit, fmt=format1, iomsg=iomsg, iostat=stat4 ) dtv%c4

   if ( ( stat1 /= 0 ) .or. ( stat2 /= 0 ) .or. ( stat3 /= 0 ) .or. ( stat4 /= 0 ) )        error stop 7_4

end subroutine



! Extensions to introduce derived type parameters:
! type: fourintegers - added parameters (kf1,kf2,kf3,kf4) to invoke with (1,2,4,8) / declare with (1,2,4,8) - 13 changes