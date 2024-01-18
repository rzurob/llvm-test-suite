!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamenest003.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 30, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : rename a public operator unary nested module, multiple renames
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type baseobj
      integer :: x = -999
      integer :: y = -999
      contains
         procedure :: mybadd
         generic :: operator(.add.) => mybadd
   end type

   interface operator(.add.)
   		module procedure mybadd2
   end interface

   contains

   function mybadd ( a )
      class(baseobj), intent(in) :: a

      type(baseobj) :: mybadd

      mybadd%y = a%x + a%y
      mybadd%x = a%x + a%y + 1000

      print *, 'mybadd'

   end function

   function mybadd2 ( a )
      integer, intent(in) :: a
      integer :: mybadd2

      mybadd2 = a + 1000


      print *, 'mybadd2'

   end function

end module

module m2
use m, operator (.plus.) => operator(.add.)
end module
module m3
use m2, operator (.addit.) => operator(.plus.)
end module
module m4
use m3, operator (.plusit.) => operator(.addit.)
end module

program mainprog
   use m4, operator (.addtogether.) => operator(.plusit.)

   type(baseobj), target      :: b1
   type(baseobj), pointer     :: b2
   type(baseobj), allocatable :: b3
   integer :: i1=5,i2=5,i3=0
   b1 = baseobj ( 100, 200 )

   allocate ( b3, source = baseobj () )

   b3 = .add. b1
   print *, b3%x

   i3 = .addtogether. i2
   print *, i3

end program
