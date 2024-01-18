!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: zerosized002a.f
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
!*  DESCRIPTION                : Testing: Ensure zero-sized type will invoke DTIO for array(sequential access read)
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

    type :: nonemptybase
       character(1) :: c = 'c'
    end type

end module

program zerosized002a
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

      subroutine unformattedReadNonEmpty (dtv, unit, iostat, iomsg)
      use m
         class(nonemptybase), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine

   end interface

   ! declaration of variables

   integer :: stat

   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:,:)
   type (base)              :: b3(2)

   class(emptybase), allocatable :: e1(:,:)
   class(emptybase), pointer     :: e2(:)
   type (emptybase)              :: e3(2)

   class(nonemptybase), allocatable :: n1(:)
   class(nonemptybase), pointer     :: n2(:,:)
   type (nonemptybase)              :: n3(0)

   ! allocation of variables

   allocate ( b1(3), b2(3,3) )
   allocate ( e1(2,2), e2(5) )
   allocate ( n1(0), n2(0,0) )

   open(1, file='zerosized002a.data', access='sequential', form='unformatted')

   ! write some arbitrary records to file, so end of file will not be reached.
   write (1, iostat = stat) "a"
   write (1, iostat = stat) "b"
   write (1, iostat = stat) "c"
   write (1, iostat = stat) "d"
   write (1, iostat = stat) "e"
   write (1, iostat = stat) "f"
   write (1, iostat = stat) "g"
   write (1, iostat = stat) "h"
   write (1, iostat = stat) "i"

   rewind 1

   read (1, iostat = stat)  b1
   if ( stat /= 999 ) error stop 1_4
   read (1, iostat = stat)  e1
   if ( stat /= 998 ) error stop 2_4
   read (1, iostat = stat)  n1                !<- shall not call DTIO since no effective items
   if ( stat /= 0 )   error stop 3_4

   read (1, iostat = stat)  b2
   if ( stat /= 999 ) error stop 4_4
   read (1, iostat = stat)  e2
   if ( stat /= 998 ) error stop 5_4
   read (1, iostat = stat)  n2                !<- shall not call DTIO since no effective items
   if ( stat /= 0 )   error stop 6_4

   read (1, iostat = stat)  b3
   if ( stat /= 999 ) error stop 7_4
   read (1, iostat = stat)  e3
   if ( stat /= 998 ) error stop 8_4
   read (1, iostat = stat)  n3                !<- shall not call DTIO since no effective items
   if ( stat /= 0 )   error stop 9_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iomsg=iomsg ) dtv%i

   iostat = 999    ! change to iostat so that we know DTIO is called

end subroutine

subroutine unformattedReadZero (dtv, unit, iostat, iomsg)
use m
   class(emptybase), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   iostat = 998    ! change to iostat so that we know DTIO is called

end subroutine

subroutine unformattedReadNonEmpty (dtv, unit, iostat, iomsg)
use m
   class(nonemptybase), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iomsg=iomsg ) dtv%c

   iostat = 997

end subroutine
