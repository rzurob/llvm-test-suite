!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg006.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an non-polymorphic scalar dummy argument
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

   subroutine myRead(unit, stat, msg, a, b )
      type(base), intent(inout)  :: a
      type(child), intent(inout) :: b
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      read(unit, iostat=stat, iomsg=msg) a,b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   end subroutine

end module

program dummyArg006
   use m1

   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(9)  :: cc1, cc2, cc3

   type(base)  :: b1
   type(base), pointer :: b2
   type(base), allocatable :: b3

   type(child) :: c1 = child('abc','def')
   type(child), pointer :: c2
   type(child), allocatable :: c3

   allocate( b2, b3, c2, c3 )

   cc1 = 'ABCabcdef'
   cc2 = 'DEFghijkl'
   cc3 = 'GHImnopqr'

   open (unit = 1, file ='dummyArg006.data', form='unformatted', access='sequential')

   ! unformatted I/O operations



   write (1, iostat=stat, iomsg=msg )              cc1
   write (1, iostat=stat, iomsg=msg )              cc2
   write (1, iostat=stat, iomsg=msg )              cc3

   rewind 1

   call myRead (1, stat, msg, b1, c1 )
   call myRead (1, stat, msg, b2, c2 )
   call myRead (1, stat, msg, b3, c3 )

   print *, b1
   print *, b2
   print *, b3
   print *, c1
   print *, c2
   print *, c3

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
