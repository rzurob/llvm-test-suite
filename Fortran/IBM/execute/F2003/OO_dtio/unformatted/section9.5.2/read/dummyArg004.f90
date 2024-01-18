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
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an unlimited polymorphic scalar variable
!*                               Sequential Access (with abstract class hierarchy)
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
   type, abstract :: base
      character(3) :: c = ''
      contains
         procedure, pass :: getC => getC
   end type

   type, extends(base) :: child
      character(3) :: cc = ''
      contains
         procedure, pass :: getC => getcc
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

   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function

   function getcc (a)
      class(child), intent(in) :: a
      character(3) :: getcc
      getcc = a%cc
   end function

   subroutine myRead(unit, stat, msg, a, b )
      class(*), intent(inout), allocatable :: a
      class(*), intent(inout), optional, pointer :: b
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      if ( present(b) ) then
         select type (b)
            class is (base)
      	       select type (a)
                  class is (base)
                     read(unit, iostat=stat, iomsg=msg) a,b
               end select
      	 end select
      else
      	 select type (a)
            class is (base)
               read(unit, iostat=stat, iomsg=msg)  a
         end select
      end if

   end subroutine

end module

program dummyArg004
   use m1

   ! declaration of variable
   class(*), allocatable :: b1
   class(*), pointer     :: b2
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( b1, source = child('xxx','xxx') )
   allocate ( b2, source = child('xxx','xxx') )

   open (unit = 1, file ='dummyArg004.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )              'abcABC'
   write (1, iostat=stat, iomsg=msg )              'ABCabcDEFdef'

   rewind 1

   call myRead (1, stat, msg, b1 )
   if ( (stat /= 0) .or. (msg /= 'dtio') )                               error stop 1_4

   select type ( b1 )
      class is (base)
         if ( ( b1%c /= 'abc') .or. ( b1%getC() /= 'ABC' ))              error stop 2_4
         msg = ''
      class default
         error stop 3_4
   end select

   call myRead (1, stat, msg, b1, b2 )
   if ( (stat /= 0) .or. (msg /= 'dtio') )                               error stop 4_4

   select type ( b1 )
      class is (base)
         select type (b2)
            class is (base)
               if ( ( b1%c /= 'ABC') .or. ( b1%getC() /= 'abc' ) .or.    &
                    ( b2%c /= 'DEF') .or. ( b2%getC() /= 'def' ) )       error stop 5_4
               msg = ''
            class default
               error stop 6_4
         end select
      class default
         error stop 7_4
   end select

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(3) :: temp
   read (unit, iostat=iostat ) dtv%c

   if ( iostat /= 0 )           error stop 8_4

   select type (dtv)
      type is (child)
         read (unit, iostat=iostat ) dtv%cc
      class default
         error stop 9_4
   end select

   iomsg = 'dtio'

end subroutine
