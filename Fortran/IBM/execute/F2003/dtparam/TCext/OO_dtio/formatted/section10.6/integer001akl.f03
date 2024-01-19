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
!*                                        First try I, with more complex data types
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
      integer(kf1), pointer     :: c1
      integer(kf2), allocatable :: c2
      integer(kf3), pointer     :: c3
      integer(kf4), allocatable :: c4
   end type

end module


program integer001akl
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

   class(fourintegers(1,2,4,8)), allocatable  :: f1 ! tcx: (1,2,4,8)
   class(fourintegers(1,2,4,8)), pointer      :: f2 ! tcx: (1,2,4,8)
   class(fourintegers(1,2,4,8)) , allocatable :: f3(:) ! tcx: (1,2,4,8)
   class(fourintegers(1,2,4,8)) , pointer     :: f4(:) ! tcx: (1,2,4,8)

   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate (f1)
   allocate (f2)

   allocate ( f1%c1, source = 1_1 )
   allocate ( f1%c2, source = 2_2 )
   allocate ( f1%c3, source = 3_4 )
   allocate ( f1%c4, source = 4_8 )

   allocate ( f2%c1, source = 127_1 )
   allocate ( f2%c2, source = -32768_2 )
   allocate ( f2%c3, source = -2147483648_4 )
   allocate ( f2%c4, source = 9223372036854775807_8 )

   allocate ( f3(2), f4(3) )

   allocate ( f3(1)%c1, source = 1_1 )
   allocate ( f3(1)%c2, source = 2_2 )
   allocate ( f3(1)%c3, source = 3_4 )
   allocate ( f3(1)%c4, source = 4_8 )
   allocate ( f3(2)%c1, source = 127_1 )
   allocate ( f3(2)%c2, source = -32768_2 )
   allocate ( f3(2)%c3, source = -2147483648_4 )
   allocate ( f3(2)%c4, source = 9223372036854775807_8 )

   allocate ( f4(1)%c1, source = 1_1 )
   allocate ( f4(1)%c2, source = 2_2 )
   allocate ( f4(1)%c3, source = 3_4 )
   allocate ( f4(1)%c4, source = 4_8 )
   allocate ( f4(2)%c1, source = 127_1 )
   allocate ( f4(2)%c2, source = -32768_2 )
   allocate ( f4(2)%c3, source = -2147483648_4 )
   allocate ( f4(2)%c4, source = 9223372036854775807_8 )
   allocate ( f4(3)%c1, source = 1_1 )
   allocate ( f4(3)%c2, source = 2_2 )
   allocate ( f4(3)%c3, source = 3_4 )
   allocate ( f4(3)%c4, source = 4_8 )



   open (unit = 1, file ='integer001akl.1', form='formatted', access='sequential')
   open (unit = 2, file ='integer001akl.2', form='formatted', access='stream' )

   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)             f1
   write (2, *, iostat=stat, iomsg=msg)             f2
   write (1, *, iostat=stat, iomsg=msg)             f3
   write (2, *, iostat=stat, iomsg=msg)             f3, f4

   rewind 1

   read  (1, *, iostat=stat, iomsg=msg)             f2
   read  (2, *, iostat=stat, iomsg=msg, pos=1)      f1

   ! check if values are read correctly
   if ( .not. check( f1, 127_1,-32768_2,-2147483648_4,9223372036854775807_8 ) )    error stop 1_4
   if ( .not. check( f2, 1_1,2_2,3_4,4_8 ) )                                       error stop 2_4

   read  (1, *, iostat=stat, iomsg=msg)             f4(1:3:2)

   if ( (.not. check( f4(1), 1_1,2_2,3_4,4_8 ) ) .or.  &
        (.not. check( f4(2), 127_1,-32768_2,-2147483648_4,9223372036854775807_8 ) ) .or.  &
        (.not. check( f4(3), 127_1,-32768_2,-2147483648_4,9223372036854775807_8 ) ) )    error stop 3_4

   read  (2, *, iostat=stat, iomsg=msg)             f3(2:1:-1), f4((/1,3,2/))

   if ( (.not. check( f3(1), 127_1,-32768_2,-2147483648_4,9223372036854775807_8 ) ) .or.  &
        (.not. check( f3(2), 1_1,2_2,3_4,4_8 ) ) .or.  &
        (.not. check( f4(1), 1_1,2_2,3_4,4_8 ) ) .or.  &
        (.not. check( f4(2), 1_1,2_2,3_4,4_8 ) ) .or.  &
        (.not. check( f4(3), 127_1,-32768_2,-2147483648_4,9223372036854775807_8 ) ) )    error stop 4_4

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

   character(10) :: format, format1
   integer :: stat1, stat2, stat3, stat4

10 format (I3.0)
20 format (1X,I11.0)
   format = "(1X,I6.0)"
   format1 = "(1X,I20.0)"

   read (unit, 10, iomsg=iomsg, iostat=stat1 )          dtv%c1
   read (unit, fmt=format, iomsg=iomsg, iostat=stat2 )  dtv%c2
   read (unit, 20, iomsg=iomsg, iostat=stat3 )          dtv%c3
   read (unit, fmt=format1, iomsg=iomsg, iostat=stat4 ) dtv%c4

   if ( ( stat1 /= 0 ) .or. ( stat2 /= 0 ) .or. ( stat3 /= 0 ) .or. ( stat4 /= 0 ) )        error stop 5_4

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

10 format (I4.3)
20 format (1X,I11)
   format = "(1X,I6)"
   format1 = "(1X,I20)"

   write (unit, 10, iomsg=iomsg, iostat=stat1 )          dtv%c1
   write (unit, fmt=format, iomsg=iomsg, iostat=stat2 )  dtv%c2
   write (unit, 20, iomsg=iomsg, iostat=stat3 )          dtv%c3
   write (unit, fmt=format1, iomsg=iomsg, iostat=stat4 ) dtv%c4

   if ( ( stat1 /= 0 ) .or. ( stat2 /= 0 ) .or. ( stat3 /= 0 ) .or. ( stat4 /= 0 ) )        error stop 6_4

end subroutine



! Extensions to introduce derived type parameters:
! type: fourintegers - added parameters (kf1,kf2,kf3,kf4) to invoke with (1,2,4,8) / declare with (1,2,4,8) - 9 changes
