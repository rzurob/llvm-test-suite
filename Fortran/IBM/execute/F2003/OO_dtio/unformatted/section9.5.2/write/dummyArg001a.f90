!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg001a.f
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
!*                               - Try output item to be an scalar dummy argument of pointer/allocatable
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
      class(base), intent(in), allocatable :: a
      class(base), intent(in), optional, pointer :: b
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      if (.not. present(b) ) then
         write(unit, iostat=stat, iomsg=msg) a
      else
      	 write(unit, iostat=stat, iomsg=msg) a,b
      end if

      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   end subroutine

end module

program dummyArg001a
   use m1

   ! declaration of variables
   class(base), allocatable :: b1
   class(base), pointer     :: b2

   integer :: stat
   character(200) :: msg
   character(3)  :: c1
   character(9)  :: c2
   character(12) :: c3

   ! allocation of variables
   allocate ( b1, source = child('abc','def') )
   allocate ( b2, source = base('ghi') )

   open (unit = 1, file ='dummyArg001a.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   call myWrite (1, stat, msg, b1 )
   call myWrite (1, stat, msg, b1, b2 )

   allocate ( b2, source = child ('uvw','xyz') )

   call myWrite (1, stat, msg, b1, b2 )

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3

   ! check if the values are set correctly

   if ( c1 /= 'abc' )                    error stop 2_4
   if ( c2 /= 'abcdefghi' )              error stop 3_4
   if ( c3 /= 'abcdefuvwxyz' )           error stop 4_4
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
