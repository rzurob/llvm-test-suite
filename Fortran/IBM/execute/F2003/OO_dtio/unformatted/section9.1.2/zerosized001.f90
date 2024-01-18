!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: zerosized001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Ensure zero-sized type will invoke DTIO (sequential access)
!*                                        and ensure zero-length records work with DTIO
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
    type :: base
       character(0) :: i
    end type

    type :: emptybase
    end type

end module

program zerosized001
use m

   interface read(unformatted)

      subroutine unformattedRead (dtv, unit, iostat, iomsg)
      use m
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine

      subroutine unformattedReadZero (dtv, unit, iostat, iomsg)
      use m
         class(emptybase), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine unformattedWrite (dtv, unit, iostat, iomsg)
      use m
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine

      subroutine unformattedWriteZero (dtv, unit, iostat, iomsg)
      use m
         class(emptybase), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   integer :: stat

   character(200) :: msg  = ''
   character(1) :: c1, c2, c3, c4, c5

   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type (base)              :: b3

   class(emptybase), allocatable :: e1
   class(emptybase), pointer     :: e2
   type (emptybase)              :: e3

   ! allocation of variables

   allocate ( b1, b2 )
   allocate ( e1, e2 )

   open(1, file='zerosized001.data', access='sequential', form='unformatted')

   write (1, iostat = stat, iomsg = msg) b1
      if ( ( stat /= 998 ) .or. ( msg /= '' ) ) error stop 1_4
   write (1, iostat = stat, iomsg = msg) "A"
   write (1, iostat = stat, iomsg = msg) e1
      if ( ( stat /= 996 ) .or. ( msg /= '' ) ) error stop 2_4
   write (1, iostat = stat, iomsg = msg) "B"
   write (1, iostat = stat, iomsg = msg) b2
      if ( ( stat /= 998 ) .or. ( msg /= '' ) ) error stop 3_4
   write (1, iostat = stat, iomsg = msg) "C"
   write (1, iostat = stat, iomsg = msg) e2
      if ( ( stat /= 996 ) .or. ( msg /= '' ) ) error stop 4_4
   write (1, iostat = stat, iomsg = msg) "D"
   write (1, iostat = stat, iomsg = msg) b3
      if ( ( stat /= 998 ) .or. ( msg /= '' ) ) error stop 5_4
   write (1, iostat = stat, iomsg = msg) "E"
   write (1, iostat = stat, iomsg = msg) e3
      if ( ( stat /= 996 ) .or. ( msg /= '' ) ) error stop 6_4

   rewind 1

   read (1, iostat = stat, iomsg = msg) b1
      if ( ( stat /= 999 ) .or. ( msg /= '' ) ) error stop 7_4
   read (1, iostat = stat, iomsg = msg) c1
   read (1, iostat = stat, iomsg = msg) e1
      if ( ( stat /= 997 ) .or. ( msg /= '' ) ) error stop 8_4
   read (1, iostat = stat, iomsg = msg) c2
   read (1, iostat = stat, iomsg = msg) b2
      if ( ( stat /= 999 ) .or. ( msg /= '' ) ) error stop 9_4
   read (1, iostat = stat, iomsg = msg) c3
   read (1, iostat = stat, iomsg = msg) e2
      if ( ( stat /= 997 ) .or. ( msg /= '' ) ) error stop 10_4
   read (1, iostat = stat, iomsg = msg) c4
   read (1, iostat = stat, iomsg = msg) b3
      if ( ( stat /= 999 ) .or. ( msg /= '' ) ) error stop 11_4
   read (1, iostat = stat, iomsg = msg) c5
   read (1, iostat = stat, iomsg = msg) e3
      if ( ( stat /= 997 ) .or. ( msg /= '' ) ) error stop 12_4

   ! check if the values are read properly

   if ( c1 /= "A" ) error stop 13_4
   if ( c2 /= "B" ) error stop 14_4
   if ( c3 /= "C" ) error stop 15_4
   if ( c4 /= "D" ) error stop 16_4
   if ( c5 /= "E" ) error stop 17_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iomsg=iomsg ) dtv%i    !<- zero length character

   iostat = 999    ! change the iostat so that we know DTIO is called

end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iomsg=iomsg ) dtv%i    !<- zero length character

   iostat = 998    ! change the iostat so that we know DTIO is called

end subroutine

subroutine unformattedReadZero (dtv, unit, iostat, iomsg)
use m
   class(emptybase), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type (dtv)
      type is (emptybase)
         read (unit, iomsg=iomsg) dtv
   end select

   iostat = 997    ! change the iostat so that we know DTIO is called

end subroutine


subroutine unformattedWriteZero (dtv, unit, iostat, iomsg)
use m
   class(emptybase), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type (dtv)
      type is (emptybase)
         write (unit, iomsg=iomsg) dtv
   end select

   iostat = 996    ! change the iostat so that we know DTIO is called

end subroutine
