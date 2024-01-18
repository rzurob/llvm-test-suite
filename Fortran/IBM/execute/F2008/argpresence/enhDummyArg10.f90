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
      contains
         procedure(GetPosition_d), pass, deferred :: GetPosition
   end type SHAPE_

   abstract interface
      function GetPosition_d(S, counter)
         import SHAPE_
         class(SHAPE_), intent(in) :: S
         integer, intent(inout) :: counter
         integer, allocatable :: GetPosition_d
      end function GetPosition_d
   end interface

   type, extends(SHAPE_) :: SPHERE_
      integer :: radius
      contains
         procedure, pass :: GetPosition=>GetPosition_SPHERE
   end type SPHERE_

   type, extends(SHAPE_) :: PLANE_
      integer :: normal
      contains
         procedure, pass :: GetPosition=>GetPosition_PLANE
   end type PLANE_

   contains

   function GetStatus(S, counter)
      class(STATUS_), intent(in) :: S
      integer, intent(inout) :: counter
      logical, allocatable :: GetStatus
      logical, allocatable :: stat
      allocate(GetStatus)
      allocate(stat)
      counter = counter + 1
      GetStatus = stat
   end function GetStatus

   function GetPosition_SPHERE(S, counter)
      class(SPHERE_), intent(in) :: S
      integer, intent(inout) :: counter
      integer, allocatable :: GetPosition_SPHERE
      integer, allocatable :: pos
      allocate(GetPosition_SPHERE)
      allocate(pos)
      counter = counter + 1
      GetPosition_SPHERE = pos
      deallocate(GetPosition_SPHERE)
   end function GetPosition_SPHERE

   function GetPosition_PLANE(S, counter)
      class(PLANE_), intent(in) :: S
      integer, intent(inout) :: counter
      integer, allocatable :: GetPosition_PLANE
      integer, allocatable :: pos
      allocate(GetPosition_PLANE)
      allocate(pos)
      counter = counter + 1
      GetPosition_PLANE = pos
   end function GetPosition_PLANE

end module testM

program main

   use testM
   implicit none

   type(SPHERE_) :: sphere(3)
   type(PLANE_) :: plane(3)
   integer :: counter

      counter = 0
      call sub()

      call sub(sphere(1)%GetPosition(counter))
      if (counter > 1) ERROR STOP 101
      call sub(plane(1)%GetPosition(counter))
      if (counter > 2) ERROR STOP 102
      call sub(sphere(1)%GetPosition(counter), sphere(1)%pos%statusp%GetStatus(counter))
      if (counter > 4) ERROR STOP 104
      call sub(plane(1)%GetPosition(counter), plane(1)%pos%statusp%GetStatus(counter))
      if (counter > 6) ERROR STOP 106

      call sub(sphere(3)%GetPosition(counter))
      if (counter > 7) ERROR STOP 107
      call sub(plane(3)%GetPosition(counter))
      if (counter > 8) ERROR STOP 108
      call sub(sphere(3)%GetPosition(counter), sphere(3)%pos%statusp%GetStatus(counter))
      if (counter > 10) ERROR STOP 110
      call sub(plane(3)%GetPosition(counter), plane(3)%pos%statusp%GetStatus(counter))
      if (counter > 12) ERROR STOP 112

   contains

   subroutine sub(p1, p2)
      integer, optional :: p1
      logical, optional :: p2
      print *,present(p1),present(p2)
   end subroutine sub

end program main

