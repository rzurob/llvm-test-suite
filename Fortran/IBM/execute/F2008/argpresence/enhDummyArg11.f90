!*  ============================================================================
!*
!*  TEST CASE NAME             : enhDummyArg10.f
!*
!*  DATE                       : 2011-08-15
!*
!*  PRIMARY FUNCTIONS TESTED   : Enhancement to determining dummy argument presence
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 386700
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Argument using functions with side effect
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module testM

   type :: STATUS_
      contains
         procedure, pass :: GetStatus
   end type STATUS_

   type :: POSITION_
      integer :: x, y, z
      type(STATUS_) :: statusp
   end type POSITION_

   type, abstract :: SHAPE_
      type(POSITION_), pointer :: pos
      integer :: color
   end type SHAPE_

   type, extends(SHAPE_) :: SPHERE_
      integer :: radius
   end type SPHERE_

   contains

   function GetStatus(S)
      class(STATUS_), intent(in) :: S
      integer, allocatable :: GetStatus
      integer, allocatable :: stat
      stat = 1003
      GetStatus = stat
   end function GetStatus

end module testM

program main

   use testM
   implicit none

   type(SPHERE_) :: sphere(3)
   integer :: counter

   counter = 1
   call sub()
   call sub(sphere(increment(counter))%pos%statusp%GetStatus())
   if (counter > 2) ERROR STOP 101

   contains

   subroutine sub(p)
      integer, optional :: p
      print *,present(p)
      if (present(p)) print *,p
   end subroutine sub

   integer function increment(ctr)
      integer, optional, intent(in) :: ctr
      print *,ctr
      counter = counter + 1
      increment = counter
   end function increment

end program main

