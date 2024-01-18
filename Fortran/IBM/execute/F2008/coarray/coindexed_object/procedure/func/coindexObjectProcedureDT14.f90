!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectProcedureDT14.f
!*
!*  DATE                       : 2011-05-20
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coindex object Procedure Calls
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type
!*
!*  REFERENCE                  : Feature Number 388003
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the coindex object procedure calls of derived type
!*
!*  Use of coarrays as drived type in a module.
!*  Different types (character, integer, real and complex) of derived type components
!*  User defined assignment
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   ! Data type
   type data_t
      character(len=3) :: ch
      integer :: i
      real :: r
      complex :: c

      contains
         procedure, pass :: assignData=>assignData_
         procedure, pass :: printData=>printData_
   end type data_t

   contains

   subroutine assignData_(A1, A2, image2)
      class(data_t), intent(inout) :: A1[*]
      type(data_t), intent(in) :: A2[*]
      integer, intent(in) :: image2

      A1%ch = A2[image2]%ch
      A1%i = A2[image2]%i
      A1%r = A2[image2]%r
      A1%c = A2[image2]%c
      print *,"ASSIGNING: "
      call A1%printData(this_image())
   end subroutine assignData_

   subroutine printData_(A, image)
      class(data_t), intent(inout) :: A
      integer, intent(in) :: image

      print *,"Image: ",image
      print *,"   ",A%ch
      print *,"   ",A%i
      print *,"   ",A%r
      print *,"   ",A%c
   end subroutine printData_

   subroutine verifyAssignment(A, image)
      type(data_t), intent(inout) :: A[*]
      integer, intent(in) :: image

      if (LGT(A%ch, 'ABC')) then
         print *,A%ch," .NE. ",'ABC'
         ERROR STOP 101
        else if (LLT(A%ch, 'ABC')) then
         print *,A%ch," .NE. ",'ABC'
         ERROR STOP 101
      else if (A%i .NE. image+101) then
         print *,A%i," .NE. ",image+101
         ERROR STOP 102
      else if (A%r .NE. image+101.101) then
         print *,A%r," .NE. ",image+101.101
         ERROR STOP 103
      else if (A%c .NE. (image+101.101, image+101.101)) then
         print *,A%c," .NE. ",(image+101.101, image+101.101)
         ERROR STOP 104
      end if
      print *,"VERIFIED: ",image,":A: ",A
   end subroutine verifyAssignment

end module DT

program main

   use DT

   implicit none

   type(data_t), save :: d[*], d2[*]
   integer :: image

   image = this_image()

   d2%ch = 'ABC'
   d2%i = 101
   d2%r = 101.101
   d2%c = (101.101, 101.101)

   call d%assignData(d2, image)
   print *,"ASSIGNED: "
   call d%printData(image)
   call verifyAssignment(d, image)

   SYNC ALL

end program main

