!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: end003.f
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
!*  DESCRIPTION                : Testing: Section 9.10: Error, end-of-record, and end-of-file conditions
!*                               - use both end and err specifiers with end of file conditions in a I/O operations,
!*                                 and see if the end branch will be taken correctly, with endfile statement
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


program end003
   use m1
   use ISO_FORTRAN_ENV

   ! declaration of variables
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2
   type(child), allocatable :: b3
   type(child), pointer     :: b4(:)

   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate (b1(3), source = (/ child('a',1), child('b',2), child('c',3) /) )
   allocate (b2, source = base ('A') )
   allocate (b3, source = child('B',2) )
   allocate (b4(-1:1), source = (/ child('d',10), child('e',11), child('f',12) /) )

   open (unit = 1, file ='end003.1', form='unformatted', access='sequential')
   open (unit = 2, file ='end003.2', form='unformatted', access='stream')

   ! unformatted I/O operations

   write ( 1, iostat = stat, iomsg = msg )     b2, b2, b2
   write ( 2, iostat = stat, iomsg = msg )     b1(1:3:2)

   rewind    1
   endfile   1
   backspace 1

   read ( 1, end = 10, err = 11, iostat = stat, iomsg = msg )          b2        !<- explicit ENDFILE statement used
11 error stop 1_4
10 if ( stat /= IOSTAT_END ) error stop 2_4

   read ( 2, end = 20, err = 21, iostat = stat, iomsg = msg, pos=1 )   b4        !<- end of file reached in stream access
21 error stop 3_4
20 if ( stat /= IOSTAT_END ) error stop 4_4

   rewind 1

   write ( 1, iostat = stat, iomsg = msg )             b2
   rewind 1
   read  ( 1, iostat = stat, iomsg = msg, end=11 )     b3        !<- end of record reached,
                                                                 !   should not branch to target specified by end=
                                                                 !   and it should position at the beginning of next rec

   read  ( 1, iostat = stat, iomsg = msg, end=30, err=31 ) b3    !<- end of file reached
31 error stop 5_4
30 if ( stat /= IOSTAT_END ) error stop 6_4

   rewind 2

   write ( 2, iostat = stat, iomsg = msg )               b3
   endfile 2
   read ( 2, end = 40, err = 41, iostat = stat, iomsg = msg )          b3
41 error stop 7_4
40 if ( stat /= IOSTAT_END ) error stop 8_4

   read ( 2, end = 50, err = 51, iostat = stat, iomsg = msg, pos=2 )   b3
51 error stop 9_4
50 if ( stat /= IOSTAT_END ) error stop 10_4

   ! close the file appropriately

   close ( 1, status ='delete' )
   close ( 2, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type (dtv)
      type is (base)
         read (unit, iomsg=iomsg, iostat=iostat ) dtv
      type is (child)
         read (unit, iomsg=iomsg, iostat=iostat ) dtv
   end select

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type (dtv)
      type is (base)
         write (unit, iomsg=iomsg, iostat=iostat ) dtv
      type is (child)
         write (unit, iomsg=iomsg, iostat=iostat ) dtv
   end select

end subroutine

