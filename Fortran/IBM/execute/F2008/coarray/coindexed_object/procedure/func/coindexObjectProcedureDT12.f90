!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectProcedureDT12.f
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
!*  Uses corank of 1.
!*  Use of coarrays as drived type in a module.
!*  Global assignment.
!*  Works with only even number of images.
!*  The extra or less number of images will be ignored.
!*  Data from images 1,3,... is assigned to images 2,4,...
!*  Data assigned is verified by images 1,3,...
!*  External procedure
!*
!*  Different derived type subtrings
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   ! Data type
   type data_t
      character(3) :: name_1
      character(6) :: name_2
      character(12) :: name_3
   end type data_t

   contains

   subroutine assignData(A, image)
      type(data_t), intent(inout) :: A[*]
      integer, intent(in) :: image

      A[image]%name_1 = 'ABC'
      A[image]%name_2 = 'ABCDEF'
      A[image]%name_3 = 'ABCDEFGHIJKL'

      print *,"ASSIGNED: ",image,":A: ",A[image]
   end subroutine assignData

   subroutine verifyAssignment(A, image)
      type(data_t), intent(inout) :: A[*]
      integer, intent(in) :: image

      if (LGT(A[image]%name_1, 'ABC')) then
         print *,A[image]%name_1," .NE. ",'ABC'
         ERROR STOP 103
      else if (LLT(A[image]%name_1, 'ABC')) then
         print *,A[image]%name_1," .NE. ",'ABC'
         ERROR STOP 103
      else if (LGT(A[image]%name_2, 'ABCDEF')) then
         print *,A[image]%name_2," .NE. ",'ABCDEF'
         ERROR STOP 103
      else if (LLT(A[image]%name_2, 'ABCDEF')) then
         print *,A[image]%name_2," .NE. ",'ABCDEF'
         ERROR STOP 103
      else if (LGT(A[image]%name_3, 'ABCDEFGHIJKL')) then
         print *,A[image]%name_3," .NE. ",'ABCDEFGHIJKL'
         ERROR STOP 103
      else if (LLT(A[image]%name_3, 'ABCDEFGHIJKL')) then
         print *,A[image]%name_3," .NE. ",'ABCDEFGHIJKL'
         ERROR STOP 103
      end if
      print *,"VERIFIED: ",image,":A: ",A
   end subroutine verifyAssignment

   subroutine verifyFirstNames(name_1, name_2, name_3, image)
      character(len=1), intent(inout) :: name_1
      character(len=3), intent(inout) :: name_2
      character(len=6), intent(inout) :: name_3
      integer, intent(in) :: image

      if (LGT(name_1, 'A')) then
         print *,name_1," .NE. ",'A'
         ERROR STOP 201
      else if (LLT(name_1, 'A')) then
         print *,name_1," .NE. ",'A'
         ERROR STOP 201
      else if (LGT(name_2, 'ABC')) then
         print *,name_2," .NE. ",'ABC'
         ERROR STOP 201
      else if (LLT(name_2, 'ABC')) then
         print *,name_2," .NE. ",'ABC'
         ERROR STOP 201
      else if (LGT(name_3, 'ABCDEF')) then
         print *,name_3," .NE. ",'ABCDEF'
         ERROR STOP 201
      else if (LLT(name_3, 'ABCDEF')) then
         print *,name_3," .NE. ",'ABCDEF'
         ERROR STOP 201
      end if
      print *,"VERIFIED: ",image,":First Name: ",name_1
      print *,"VERIFIED: ",image,":First Name: ",name_2
      print *,"VERIFIED: ",image,":First Name: ",name_3
   end subroutine verifyFirstNames

   subroutine verifyLastNames(name_1, name_2, name_3, image)
      character(len=1), intent(inout) :: name_1
      character(len=3), intent(inout) :: name_2
      character(len=6), intent(inout) :: name_3
      integer, intent(in) :: image

      if (LGT(name_1, 'C')) then
         print *,name_1," .NE. ",'C'
         ERROR STOP 201
      else if (LLT(name_1, 'C')) then
         print *,name_1," .NE. ",'C'
         ERROR STOP 201
      else if (LGT(name_2, 'DEF')) then
         print *,name_2," .NE. ",'DEF'
         ERROR STOP 201
      else if (LLT(name_2, 'DEF')) then
         print *,name_2," .NE. ",'DEF'
         ERROR STOP 201
      else if (LGT(name_3, 'GHIJKL')) then
         print *,name_3," .NE. ",'GHIJKL'
         ERROR STOP 201
      else if (LLT(name_3, 'GHIJKL')) then
         print *,name_3," .NE. ",'GHIJKL'
         ERROR STOP 201
      end if
      print *,"VERIFIED: ",image,":Last Name: ",name_1
      print *,"VERIFIED: ",image,":Last Name: ",name_2
      print *,"VERIFIED: ",image,":Last Name: ",name_3
   end subroutine verifyLastNames

end module DT

program main

   use DT

   implicit none

   integer image, next_image

   type(data_t), save :: d[*]
   image = this_image()

   call assignData(d, image)
   call verifyAssignment(d, image)
   call verifyFirstNames(d%name_1(1:1), d%name_2(1:3), d%name_3(1:6), image)
   call verifyLastNames(d%name_1(3:3), d%name_2(4:6), d%name_3(7:12), image)

   SYNC ALL

end program main
