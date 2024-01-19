!*  ============================================================================
!*
!*  DATE                       : 2011-03-15
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coindex object assignment statement
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type
!*
!*  REFERENCE                  : Feature Number 386924
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  A simple linked list with insert, removeLast and verifyList:
!*  Uses corank of 1.
!*  Assignment using derived type with pointer components.
!*  Copying/moving data among images. Works with only even number of images.
!*  The extra or less number of images will be ignored. Even image copies
!*  data into odd images:
!*  2,4,6... ==> 1,3,5...
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   implicit none

   type :: Node
      private
      integer data
      type(Node), POINTER :: next => null()
      type(Node), POINTER :: prev => null()
   end type Node

   type :: List
      type(Node), POINTER :: head => null()
      type(Node), POINTER :: current => null()
      contains
   end type List

   contains

   subroutine insert(L, data)
      type(List), intent(inout) :: L
      type(Node), POINTER :: node
      integer, intent(in) :: data

      if (associated(L%head)) then
         allocate(node)
         L%current%next => node
         node%prev => L%current
      else
         allocate(node)
         L%head => node
         L%head%prev => L%head
      end if

      L%current => node
      L%current%data = data
   end subroutine insert

   subroutine removeLast(L)
      type(List), intent(inout) :: L
      type(Node), POINTER :: prev

      if (associated(L%current, L%head)) then
         deallocate (L%head)
         nullify (L%head)
      else if (associated(L%current)) then
         prev => L%current%prev
         deallocate (L%current)
         nullify (L%current)
         L%current => prev
      end if
   end subroutine removeLast

   subroutine verifyList(L, image)
      type(List), intent(in) :: L
      integer, intent(in) :: image
      type(Node), POINTER :: node
      type(Node), POINTER :: next
      integer i

      i = 1
      node => L%head
      do while (associated(node))
         if (node%data .NE. i+image+300) then
            print *,image,":",node%data," .NE. ",i+image+300
            ERROR STOP 101
         end if
         print *,image,":",node%data
         node => node%next
         i = i + 1
      end do
   end subroutine verifyList

end module

program main

   use DT
   implicit none
   integer, PARAMETER :: N = 5
   integer i, image
   type(List), save :: list[*]

   image = this_image()
   if (mod(image, 2) .EQ. 0) then
      do i = 1, N
         call insert(list[image-1], i+image-1+300)
      end do

      call verifyList(list[image-1], image-1);
      do i = 1, N
         call removeLast(list[image-1])
      end do
   end if

   SYNC ALL

end program main

