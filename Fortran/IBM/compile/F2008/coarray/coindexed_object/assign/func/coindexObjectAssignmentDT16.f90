!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectAssignmentDT16.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-03-21
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coindex object assignment statement
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type
!*
!*  REFERENCE                  : Feature Number 386924
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
!*  Uses corank of 1.
!*  Assignment using derived type co-indexed object with ultimate allocatable component.
!*  Should give an error message.
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   type mountain
      character, ALLOCATABLE :: name(:)
   end type mountain

end module DT

program main

   use DT

   implicit none

   integer image
   type(mountain), save :: M1[*]
   type(mountain), save :: M2[*]

   image = this_image()

   ! Odd images
   if (mod(image, 2) .EQ. 1) then
      allocate(M1%name(3))
      allocate(M2[image]%name(3))
      M1%name = (/ 'M','T','N' /)
      M2[image] = M1                    ! Error
      M2 = M1[image]                    ! No Error

      print *,"Image:",image,":M1 :",M1%name
      print *,"Image:",image,":M2 :",M2[image]%name

      deallocate(M1%name)
      deallocate(M2%name)
   end if

end program main

