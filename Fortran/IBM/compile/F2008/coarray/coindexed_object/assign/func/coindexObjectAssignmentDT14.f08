!*  ============================================================================
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
!*  Uses corank of 1 and 2.
!*  Uses different derived type components (derived type, real, character, complex and logical)
!*  to access (read/write) the co-indexed object.
!*  Should produce error messages.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   type coindex
      integer :: rank
   end type coindex

   type rank
      real :: rank_real
      character :: rank_character
      complex :: rank_complex
      logical :: rank_logical
      type(coindex) :: ci
   end type rank

   type mountain
      integer :: id
   end type mountain

end module DT

program main

   use DT

   implicit none

   integer image
   type(rank) :: R
   type(mountain), save :: Mtn[*]
   type(mountain), save :: Mtns[4,*]

   image = this_image()

   Mtn[R%ci%rank]%id = 123
   print *,image,":",Mtn[R%ci%rank]%id
   Mtn[R%rank_real]%id = 123
   print *,image,":",Mtn[R%rank_real]%id
   Mtn[R%rank_character]%id = 123
   print *,image,":",Mtn[R%rank_character]%id
   Mtn[R%rank_complex]%id = 123
   print *,image,":",Mtn[R%rank_complex]%id
   Mtn[R%rank_logical]%id = 123
   print *,image,":",Mtn[R%rank_logical]%id

   Mtns[1, R%ci%rank]%id = 123
   print *,image,":",Mtns[1, R%ci%rank]%id
   Mtns[1, R%rank_real]%id = 123
   print *,image,":",Mtns[1, R%rank_real]%id
   Mtns[1, R%rank_character]%id = 123
   print *,image,":",Mtns[1, R%rank_character]%id
   Mtns[1, R%rank_complex]%id = 123
   print *,image,":",Mtns[R%rank_complex]%id
   Mtns[1, R%rank_logical]%id = 123
   print *,image,":",Mtns[1, R%rank_logical]%id

   Mtns[R%ci%rank, 1]%id = 123
   print *,image,":",Mtns[R%ci%rank, 1]%id
   Mtns[R%rank_real, 1]%id = 123
   print *,image,":",Mtns[R%rank_real, 1]%id
   Mtns[R%rank_character, 1]%id = 123
   print *,image,":",Mtns[R%rank_character, 1]%id
   Mtns[R%rank_complex, 1]%id = 123
   print *,image,":",Mtns[R%rank_complex, 1]%id
   Mtns[R%rank_logical, 1]%id = 123
   print *,image,":",Mtns[R%rank_logical, 1]%id

end program main
