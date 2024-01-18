!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: real001.f
! %VERIFY: real001.out:real001.vf
! %STDIN:
! %STDOUT: real001.out
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
!*  DESCRIPTION                : Testing: Section 10.6.1.2: Real Editing
!*                                        Try different real editing descriptor in child data transfer stmt
!*                                        First try F editing
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
   type threereals
      real(4)  :: c1 = 0.0
      real(8)  :: c2 = 0.0
      real(16) :: c3 = 0.0
   end type
end module


program real001
   use m1

   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import threereals
         class(threereals), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import threereals
         class(threereals), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   procedure(logical) :: precision_r4
   procedure(logical) :: precision_r8
   procedure(logical) :: precision_r16
   class(threereals), allocatable :: f1
   class(threereals), pointer     :: f2

   integer :: stat
   character(200) :: msg
   character(18)  :: c1, c2

   ! allocation of variables

   allocate (f1, source = threereals(1.0,2.0,3.0))                                          !<- basic test
   allocate (f2, source = threereals(tiny(f2%c1),1.3E1,102.6D-1))                           !<- basic test


   open (unit = 1, file ='real001.1', form='formatted', access='sequential')
   open (unit = 2, file ='real001.2', form='formatted', access='stream', status='replace')

   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)             f1
   write (2, *, iostat=stat, iomsg=msg)             f2

   rewind 1
   rewind 2

   read (1, fmt="(A)")          c1
   read (2, fmt="(A)")          c2

   if ( c1 /= "  1.0  2.000  3.00" )  error stop 1_4
   if ( c2 /= "  0.0 13.000 10.26" )  error stop 2_4

   rewind 1
   rewind 2

   deallocate ( f1, f2 )
   allocate   ( f1, f2 )

   read (1, *, iostat=stat, iomsg=msg)             f1
   read (2, *, iostat=stat, iomsg=msg)             f2

   print *, f1
   print *, f1%c1, f1%c2, f1%c3
   print *, f2
   print *, f2%c1, f2%c2, f2%c3

   ! close the file appropriately

   close ( 1, status ='delete' )
   close ( 2, status ='delete' )

contains

   logical function check(dtv, a, b, c, d)
      class(threereals), intent(in) :: dtv
      integer(1), intent(in) :: a
      integer(2), intent(in) :: b
      integer(4), intent(in) :: c
      integer(8), intent(in) :: d

      check = ( ( dtv%c1 == a ) .and. ( dtv%c2 == b ) .and. ( dtv%c3 == c ))

   end function

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
   class(threereals), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(9) :: format
   integer :: stat1, stat2, stat3

10 format (F3.0)
20 format (1X,F5.0)                 !<- d is zero, and it should have no effect since input fields have decimal
   format = "(1X,F6.0)"

   read (unit, 10, iomsg=iomsg, iostat=stat1 )          dtv%c1
   read (unit, fmt=format, iomsg=iomsg, iostat=stat2 )  dtv%c2
   read (unit, 20, iomsg=iomsg, iostat=stat3 )          dtv%c3

   if ( ( stat1 /= 0 ) .or. ( stat2 /= 0 ) .or. ( stat3 /= 0 )  )        error stop 5_4

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
   class(threereals), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(9) :: format
   integer :: stat1, stat2, stat3

10 format (F4.1)                 !<- list-directed read does not position file with 1 space
20 format (1X,F5.2)
   format = "(1X,F6.3)"

   write (unit, 10, iomsg=iomsg, iostat=stat1 )          dtv%c1
   write (unit, fmt=format, iomsg=iomsg, iostat=stat2 )  dtv%c2
   write (unit, 20, iomsg=iomsg, iostat=stat3 )          dtv%c3

   if ( ( stat1 /= 0 ) .or. ( stat2 /= 0 ) .or. ( stat3 /= 0 ) )        error stop 6_4

end subroutine

