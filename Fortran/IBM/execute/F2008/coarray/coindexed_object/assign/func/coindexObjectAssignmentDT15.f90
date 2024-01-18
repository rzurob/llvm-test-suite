!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectAssignmentDT15.f
!*
!*  DATE                       : 2011-03-21
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
!*  Uses corank of 1.
!*  Assignment using derived type co-array.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   type mountain
      character :: name(3)
      integer :: id
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
      M1[image]%name = (/'M','T','N' /)
      M1[image]%id = 111456111
      M2[image]%name = M1%name
      M2[image]%id = M1[image]%id

      print *,"Image:",image,":M1 :",M1%name,M1[image]%id
      print *,"Image:",image,":M2 :",M2%name,M2[image]%id
   end if

end program main
