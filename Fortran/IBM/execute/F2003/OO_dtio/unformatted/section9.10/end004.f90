!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: end004.ksh
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.10: Error, end-of-record, and end-of-file conditions
!*                               - If an end-of-file condition occurs during execution of an input/output
!*                                 statement that contains enither an END= nor and IOSTAT= specifier,
!*                                 execution of the program is terminated (sequential access)
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
      character(1) :: c = ''
   end type

   type, extends(base) :: child
      integer(4) :: cc = -1
   end type

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module


program end004
   use m1
   use ISO_FORTRAN_ENV

   ! declaration of variables
   class(base), allocatable :: b1(:)

   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate (b1(3), source = (/ child('a',1), child('b',2), child('c',3) /) )

   open (unit = 1, file ='end004.1', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write ( 1, iostat = stat, iomsg = msg )     b1(1:3:2)

   rewind 1

   print *, "Before Program termination"
   read ( 1, iomsg = msg )                     b1       !<- end of file reached and no iostat= nor end=, program terminates here
   print *, "ERROR"

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iomsg=iomsg, iostat=iostat ) dtv%c

   select type(dtv)
      type is (child)
         read (unit, iomsg=iomsg, iostat=iostat ) dtv%cc
   end select

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iomsg=iomsg, iostat=iostat ) dtv%c

   select type(dtv)
      type is (child)
         write (unit, iomsg=iomsg, iostat=iostat ) dtv%cc
   end select

end subroutine

