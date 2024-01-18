!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectAssignmentDT17.f
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
!*  Assignment of derived type coindex object using defined assignment.
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
   type(mountain), save :: M1[*]
   type(mountain), save :: M2[*]
   type(mountain), save :: M3[*]

   interface assignment(=)
      subroutine myassignment(d2, d1)
         import mountain
         character, intent(in) :: d1(7)
         type(mountain), intent(out) :: d2[*]
      end subroutine
   end interface

   image = this_image()

   ! Odd images
   if (mod(image, 2) .EQ. 1) then
      M1[image] = (/'M','T','N','_','1','I','D' /)                     ! MTN I=73 D=68
      M2[image] = (/'M','T','N','_','2','A','Z' /)                     ! MTN A=65 D=90
      M3[image] = (/'M','T','N','_','3','S','S' /)                     ! MTN I=83 D=83

      print *,"Image:",image,":",M1[image]%name,M1[image]%id           ! MTN_1 141
      print *,"Image:",image,":",M2[image]%name,M2[image]%id           ! MTN_2 155
      print *,"Image:",image,":",M3[image]%name,M3[image]%id           ! MTN_2 166
   end if

end program main

subroutine myassignment(d2, d1)

   use DT, only:mountain
   character, intent(in) :: d1(7)
   type(mountain), intent(out) :: d2[*]

   d2[image]%name = d1(1:5)
   d2[image]%id = iachar(d1(6)) + iachar(d1(7))

end subroutine

