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
! %GROUP: dummyArg006a.f
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
!*                               - Try input item to be an non-polymorphic scalar dummy argument with pointer/allocatable attribute
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
      character(3) :: c = 'xxx'
   end type
   type, extends(base) :: child
      character(3) :: cc = 'xxx'
   end type

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

contains

   subroutine myReadA(unit, stat, msg, a, b )
      type(base), allocatable, intent(inout)  :: a
      type(child), allocatable, intent(inout) :: b
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      read(unit, iostat=stat, iomsg=msg) a,b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   end subroutine

   subroutine myReadP(unit, stat, msg, a, b )
      type(base), pointer, intent(inout)  :: a
      type(child), pointer, intent(inout) :: b
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      read(unit, iostat=stat, iomsg=msg) a,b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   end subroutine

end module

program dummyArg006a
   use m1

   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(9)  :: cc1, cc2

   type(base), pointer :: b1
   type(base), allocatable :: b2
   type(child), pointer :: c1
   type(child), allocatable :: c2

   allocate( b1, b2, c1, c2 )

   cc1 = 'ABCabcdef'
   cc2 = 'DEFghijkl'

   open (unit = 1, file ='dummyArg006a.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )              cc1
   write (1, iostat=stat, iomsg=msg )              cc2

   rewind 1

   call myReadP (1, stat, msg, b1, c1 )
   call myReadA (1, stat, msg, b2, c2 )

   print *, b1
   print *, b2
   print *, c1
   print *, c2

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( g => dtv )
      type is (base)
         read (unit, iostat=iostat ) g%c
      type is (child)
         read (unit, iostat=iostat ) g%c, g%cc
   end select
   iomsg = 'dtioread'
end subroutine
