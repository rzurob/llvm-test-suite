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
! %GROUP: dummyArg004.f
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
!*                               - Try output item to be an non-polymorphic scalar dummy argument
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
   type base
      character(3) :: c = ''
   end type
   type, extends(base) :: child
      character(3) :: cc = ''
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

contains

   subroutine myWrite(unit, stat, msg, a, b )
      type(base), intent(in)  :: a
      type(child), intent(in) :: b
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      write(unit, iostat=stat, iomsg=msg) a,b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   end subroutine

end module

program dummyArg004
   use m1

   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(9)  :: cc1, cc2, cc3

   type(base)  :: b1 = base('ABC')
   type(base), pointer :: b2
   type(base), allocatable :: b3

   type(child) :: c1 = child('abc','def')
   type(child), pointer :: c2
   type(child), allocatable :: c3

   allocate( b2, b3, c2, c3 )

   b2%c = 'DEF'
   b3%c = 'GHI'
   c2%c = 'ghi'
   c2%cc= 'jkl'
   c3%c = 'mno'
   c3%cc= 'pqr'

   open (unit = 1, file ='dummyArg004.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   call myWrite (1, stat, msg, b1, c1 )
   call myWrite (1, stat, msg, b2, c2 )
   call myWrite (1, stat, msg, b3, c3 )

   rewind 1

   read (1, iostat=stat, iomsg=msg )              cc1
   read (1, iostat=stat, iomsg=msg )              cc2
   read (1, iostat=stat, iomsg=msg )              cc3

   ! check if the values are set correctly

   if ( cc1 /= 'ABCabcdef' )                  error stop 2_4
   if ( cc2 /= 'DEFghijkl' )                  error stop 3_4
   if ( cc3 /= 'GHImnopqr' )                  error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( g => dtv )
      type is (base)
         write (unit, iostat=iostat ) g%c
      type is (child)
         write (unit, iostat=iostat ) g%c, g%cc
   end select
   iomsg = 'dtiowrite'
end subroutine
