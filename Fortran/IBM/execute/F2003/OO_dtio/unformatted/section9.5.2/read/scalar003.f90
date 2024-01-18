!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: scalar003.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be scalar of sequence type
!*                               Sequential Access
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
      sequence
      character(3), allocatable :: c
   end type
end module


program scalar003
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         type(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   class(*), allocatable, target :: u1
   class(*), pointer             :: u2
   type(base), allocatable   :: b3
   type(base),  pointer      :: b4
   type(base)                :: b5

   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate ( u1, source = base(null()) )
   allocate ( u2, source = base(null()) )
   allocate ( b3, source = base(null()) )

   open (unit = 1, file ='scalar003.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             'abc'
   write (1, iostat=stat, iomsg=msg )             'def'
   write (1, iostat=stat, iomsg=msg )             'ghi'
   write (1, iostat=stat, iomsg=msg )             'jkl'
   write (1, iostat=stat, iomsg=msg )             'mno'

   rewind 1
   b4 => u1
   read (1, iostat=stat, iomsg=msg )             b4
   b4 => u2
   read (1, iostat=stat, iomsg=msg )             b4

   allocate ( b4, source = base(null()) )
   read (1, iostat=stat, iomsg=msg )             b3
   if (( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   read (1, iostat=stat, iomsg=msg )             b4
   if (( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   read (1, iostat=stat, iomsg=msg )             b5
   if (( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   if ( b3%c /= 'ghi') error stop 4_4
   if ( b4%c /= 'jkl') error stop 5_4
   if ( b5%c /= 'mno') error stop 6_4
   b4 => u1
   if ( b4%c /= 'abc') error stop 7_4
   b4 => u2
   if ( b4%c /= 'def') error stop 8_4
   

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1

   type(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( .not. allocated(dtv%c ) ) then
      allocate(dtv%c, source = 'xxx')
   end if

   read (unit, iostat=iostat )     dtv%c
   iomsg = 'dtioread'

end subroutine
