!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: integer002.f
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
!*  DESCRIPTION                : Testing: Section 10.6.1.1: Integer Editing
!*                                        Try different integer editing descriptor in child data transfer stmt
!*                                        First try B, O, X
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
   type fourintegers
      integer(1) :: c1 = 0
      integer(2) :: c2 = 0
      integer(4) :: c3 = 0
      integer(8) :: c4 = 0
   end type
end module


program integer002
   use m1

   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import fourintegers
         class(fourintegers), intent(in) :: dtv
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
         class(fourintegers), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   class(fourintegers), allocatable :: f1
   class(fourintegers), pointer     :: f2
   type(fourintegers) , allocatable :: f3
   type(fourintegers) , pointer     :: f4

   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate (f1, source = fourintegers(1_1,2_2,3_4,4_8))                                          !<- basic test
   allocate (f2, source = fourintegers(127_1,-32768_2,-2147483648_4,9223372036854775807_8))       !<- with maximum values
   allocate (f3, source = fourintegers(-128_1,32767_2,-2147483648_4,-9223372036854775807_8))      !<- with maximum values
   allocate (f4, source = fourintegers(-128_1,-32768_2,-2147483648_4,-9223372036854775808_8))     !<- with maximum values

   open (unit = 1, file ='integer002.1', form='formatted', access='sequential')
   open (unit = 2, file ='integer002.2', form='formatted', access='stream' )

   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)             f1
   write (2, *, iostat=stat, iomsg=msg)             f2
   write (1, *, iostat=stat, iomsg=msg)             f3, f4
   write (2, *, iostat=stat, iomsg=msg)             f1, f2, f3, f4

   rewind 1
   rewind 2

   read  (1, *, iostat=stat, iomsg=msg)             f2
   read  (2, *, iostat=stat, iomsg=msg)             f1

   ! check if values are read correctly
   if ( .not. check( f1, 127_1,-32768_2,-2147483648_4,9223372036854775807_8 ) )    error stop 1_4
   if ( .not. check( f2, 1_1,2_2,3_4,4_8 ) )                                       error stop 2_4

   read  (1, *, iostat=stat, iomsg=msg)             f1, f2

   if ( (.not. check( f1, -128_1, 32767_2,-2147483648_4,-9223372036854775807_8 ) ) .or.  &
        (.not. check( f2, -128_1,-32768_2,-2147483648_4,-9223372036854775808_8 ) ) )    error stop 3_4

   read  (2, *, iostat=stat, iomsg=msg)             f4, f3, f2, f1

   if ( (.not. check( f1, -128_1,-32768_2,-2147483648_4,-9223372036854775808_8 ) ) .or.  &
        (.not. check( f2, -128_1,32767_2,-2147483648_4,-9223372036854775807_8  ) ) .or.  &
        (.not. check( f3, 127_1,-32768_2,-2147483648_4,9223372036854775807_8   ) ) .or.  &
        (.not. check( f4, 1_1,2_2,3_4,4_8 ) ) )    error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )
   close ( 2, status ='delete' )

contains

   logical function check(dtv, a, b, c, d)
      class(fourintegers), intent(in) :: dtv
      integer(1), intent(in) :: a
      integer(2), intent(in) :: b
      integer(4), intent(in) :: c
      integer(8), intent(in) :: d

      check = ( ( dtv%c1 == a ) .and. ( dtv%c2 == b ) .and. ( dtv%c3 == c ) .and. ( dtv%c4 == d ) )

   end function

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
   class(fourintegers), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(10) :: format, format1
   integer :: stat1, stat2, stat3, stat4

10 format (1X,B8.0)                 !<- list-directed read does not position file with 1 space
20 format (1X,B32.0)
   format = "(1X,O6.0)"
   format1 = "(1X,Z16.0)"

   read (unit, 10, iomsg=iomsg, iostat=stat1 )          dtv%c1
   read (unit, fmt=format, iomsg=iomsg, iostat=stat2 )  dtv%c2
   read (unit, 20, iomsg=iomsg, iostat=stat3 )          dtv%c3
   read (unit, fmt=format1, iomsg=iomsg, iostat=stat4 ) dtv%c4

   if ( ( stat1 /= 0 ) .or. ( stat2 /= 0 ) .or. ( stat3 /= 0 ) .or. ( stat4 /= 0 ) )        error stop 5_4

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
   class(fourintegers), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(8) :: format, format1
   integer :: stat1, stat2, stat3, stat4

10 format ("|", B8)
20 format (1X,B32)
   format = "(1X,O6)"
   format1 = "(1X,Z16)"

   write (unit, 10, iomsg=iomsg, iostat=stat1 )          dtv%c1
   write (unit, fmt=format, iomsg=iomsg, iostat=stat2 )  dtv%c2
   write (unit, 20, iomsg=iomsg, iostat=stat3 )          dtv%c3
   write (unit, fmt=format1, iomsg=iomsg, iostat=stat4 ) dtv%c4

   if ( ( stat1 /= 0 ) .or. ( stat2 /= 0 ) .or. ( stat3 /= 0 ) .or. ( stat4 /= 0 ) )        error stop 6_4

end subroutine

