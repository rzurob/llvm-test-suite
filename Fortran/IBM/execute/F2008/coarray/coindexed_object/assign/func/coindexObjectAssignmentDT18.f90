!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectAssignmentDT18.f
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
!*  Assignment of derived type coindex object (multidimension) using defined assignment.
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   type mountain
      character :: name(5)
      integer :: id
   end type mountain


end module DT

program main

   use DT

   implicit none

   integer image
   type(mountain), save :: M1(3)[*]
   type(mountain), save :: M2(5)[*]
   type(mountain), save :: M3(6)[*]

   interface assignment(=)
      subroutine myassignment(d2, d1)
         import mountain
         character, intent(in) :: d1(7)
         type(mountain), intent(out) :: d2(2)[*]
      end subroutine
   end interface

   image = this_image()

   ! Odd images
   if (mod(image, 2) .EQ. 1) then
      M1(1:2)[image] = (/'M','T','N','_','1','I','D','M','A','N','G','O','A','A' /)
      M1(2:3)[image] = (/'M','T','N','_','1','I','E','M','A','N','G','O','A','A' /)

      M2(1:2)[image] = (/'M','T','N','_','2','I','D','M','A','N','G','O','A','A' /)
      M2(2:3)[image] = (/'M','T','N','_','2','I','E','M','A','N','G','O','A','A' /)
      M2(4:5)[image] = (/'M','T','N','_','2','I','F','M','A','N','G','O','A','B' /)

      M3(1:2)[image] = (/'M','T','N','_','3','I','D','M','A','N','G','O','A','A' /)
      M3(3:4)[image] = (/'M','T','N','_','3','I','E','M','A','N','G','O','A','B' /)
      M3(5:6)[image] = (/'M','T','N','_','3','I','F','M','A','N','G','O','A','C' /)

      print *,"Image:",image,":",M1[image]
      print *,"Image:",image,":",M2[image]
      print *,"Image:",image,":",M3[image]
   end if

end program main

subroutine myassignment(d2, d1)

   use DT, only:mountain
   character, intent(in) :: d1(14)
   type(mountain), intent(out) :: d2(2)[*]

   d2(1)[image]%name = d1(1:5)
   d2(1)[image]%id = iachar(d1(6)) + iachar(d1(7))
   d2(2)[image]%name = d1(8:12)
   d2(2)[image]%id = iachar(d1(13)) + iachar(d1(14))

end subroutine

