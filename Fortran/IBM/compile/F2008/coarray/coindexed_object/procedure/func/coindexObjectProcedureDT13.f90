!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectProcedureDT13.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-05-20
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coindex object Procedure Calls
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type
!*
!*  REFERENCE                  : Feature Number 388003
!*
!*  DRIVER STANZA              : xlf2003
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
!*  - Check for Codimension with deferred shape not allowed 
!*  - Check for Polymorphic co-array not yet supported
!*
!*  NOTE:
!*  When we start supporting polymorphic co-array it will only emit one error message
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
   end type data_t

   contains

   subroutine assignData(A, image)
      type(data_t), intent(inout) :: A[:]       ! Expect error message
      integer, intent(in) :: image

      A[image]%ch = 'ABC'
      A[image]%i = image+101
      A[image]%r = image+101.101
      A[image]%c = (image+101.101, image+101.101)

      print *,"ASSIGNED: ",image,":A: ",A[image]
   end subroutine assignData

   subroutine verifyAssignment(A, image)
      class(data_t), intent(inout) :: A[*]      ! Expect error message till we support the polymorphic co-array
      integer, intent(in) :: image

      if (LGT(A[image]%ch, 'ABC')) then
         print *,A[image]%ch," .NE. ",'ABC'
         ERROR STOP 101
      else if (LLT(A[image]%ch, 'ABC')) then
         print *,A[image]%ch," .NE. ",'ABC'
         ERROR STOP 101
      else if (A[image]%i .NE. image+101) then
         print *,A[image]%i," .NE. ",image+101
         ERROR STOP 102
      else if (A[image]%r .NE. image+101.101) then
         print *,A[image]%r," .NE. ",image+101.101
         ERROR STOP 103
      else if (A[image]%c .NE. (image+101.101, image+101.101)) then
         print *,A[image]%c," .NE. ",(image+101.101, image+101.101)
         ERROR STOP 104
      end if
end subroutine verifyAssignment

end module DT

program main

   use DT

   implicit none

   integer, parameter :: SIZE = 3
   integer image, next_image

   type(data_t), save :: d[*]
   image = this_image()

   call assignData(d, image)
   call verifyAssignment(d, image)
   print *,"VERIFIED: ",image,":d: ",d

   SYNC ALL

end program main
