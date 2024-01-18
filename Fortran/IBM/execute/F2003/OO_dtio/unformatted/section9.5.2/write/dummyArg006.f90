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
!*                               - Try output item to be an unlimited polymorphic scalar with pointer/allocatable attribute
!*                               Direct Access
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

   subroutine myWrite1(unit, stat, msg, recn, a, b )
      class(*), pointer, intent(in) :: a
      class(*), allocatable, intent(in), optional :: b
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      integer, intent(in) :: recn
      character(*), intent(inout) :: msg

      if ( present(b) ) then
         select type (b)
            class is (base)
      	       select type (a)
                  class is (base)
                     write(unit, iostat=stat, iomsg=msg, rec=recn) a,b
               end select
      	 end select
      else
      	 select type (a)
            class is (base)
               write(unit, iostat=stat, iomsg=msg, rec=recn)  a
         end select
      end if
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end subroutine

end module

program dummyArg006
   use m1

   ! declaration of variable
   class(*), pointer         :: b1
   class(*), allocatable     :: b2
   class(*), pointer         :: b3
   class(*), allocatable     :: b4
   integer :: stat
   character(200) :: msg
   character(6)  :: c1, c4
   character(12) :: c2
   character(3)  :: c3

   ! allocation of variables
   allocate ( b1, source = child('abc', 'def') )
   allocate ( b2, source = child('ABC', 'DEF') )
   allocate ( b3, source = base('def') )
   allocate ( b4, source = base('DEF') )

   open (unit = 1, file ='dummyArg006.data', form='unformatted', access='direct', recl=15)

   ! unformatted I/O operations

   call myWrite1 (1, stat, msg, 4, b1 )
   call myWrite1 (1, stat, msg, 3, b1, b2 )
   call myWrite1 (1, stat, msg, 2, b3)
   call myWrite1 (1, stat, msg, 1, b3, b4 )

   read (1, iostat=stat, iomsg=msg, rec=1 )              c4
   read (1, iostat=stat, iomsg=msg, rec=2 )              c3
   read (1, iostat=stat, iomsg=msg, rec=3 )              c2
   read (1, iostat=stat, iomsg=msg, rec=4 )              c1

   ! check if the values are set correctly

   if ( c1 /= 'abcdef' )              error stop 2_4
   if ( c2 /= 'abcdefABCDEF' )        error stop 3_4
   if ( c3 /= 'def' )                 error stop 4_4
   if ( c4 /= 'defDEF' )              error stop 5_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type (g => dtv)
      type is (base)
         write (unit, iostat=iostat ) g%c
      type is (child)
         write (unit, iostat=iostat ) g%c, g%cc
   end select

   iomsg = 'dtiowrite'
end subroutine
