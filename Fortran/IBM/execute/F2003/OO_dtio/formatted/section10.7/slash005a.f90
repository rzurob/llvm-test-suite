 !#######################################################################
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: slash005a.f
! %VERIFY: slash005a.1:slash005a.vf
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
!*                                        On output to a file connected for stream, a new
!*                                        empty record is created following the current record.
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

end module

program slash005a
   use m1

   ! declaration of variables

   class(base), allocatable :: f1
   class(base), allocatable :: f2(:)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'slash005a.1', access = 'stream', form='formatted' )

   ! allocation of variables

   allocate (f1, source = base('ABC') )
   allocate (f2(2), source = (/ base('ABC'), base('DEF') /) )

   ! formatted I/O operations

   ! initialize the two files with Xs

   do i =1, 10
      write (1, "(A)", iostat=stat)        '1234567890123456789'
      if ( stat /= 0 ) error stop 1_4
   end do

   rewind 1

   write ( 1, *, iostat = stat, iomsg = msg )    f1

   write ( 1, *, iostat = stat, iomsg = msg, pos=41)    f2

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: format
   format = "(A,/)"
   write (unit, format, iostat=iostat )      dtv%c1

   iomsg = 'dtiowrite'

end subroutine