!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectAssignmentDT13.f
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
!*  DRIVER STANZA              : xlf2003_r
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Uses corank of 1.
!*  Testing derived type co-arrays with pointer and allocatable components.
!*  Even images allocating and deallocating co-array derived type components for odd images.
!*  Should give an error message.
!*  Also uses derived type component (integer) to select the co-indexed objects.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   type whatRank
      integer rank
   end type whatRank

   type isTall
      logical bool
   end type isTall

   type mountain
      character(30) :: name
      integer :: id
      type(isTall) :: isTall
   end type mountain

   type mountains
      type(mountain) :: M(3)
      type(mountain), ALLOCATABLE  :: M_a(:)
      type(mountain), POINTER :: M_p(:)
   end type mountains

end module DT

program main

   use DT

   implicit none

   integer image
   type(whatRank) :: R
   type(whatRank), ALLOCATABLE :: R_a(:)
   type(whatRank), POINTER :: R_p
   type(whatRank), TARGET :: R_t
   type(mountains), save :: Mtn[*]

   image = this_image()
   ! even images allocating and deallocating co-array (Mtn) derived type components in odd images
   ! Should give an error message
   if (mod(image, 2) .EQ. 0) then
      image = image - 1
      allocate(R_a(1))
      allocate(R_p)
      R%rank = image
      R_a(1)%rank = image
      R_t%rank = image
      R_p => R_t
      Mtn[image]%M(1)%name = 'Mountain Range Himalaya'
      Mtn[image]%M(1)%id = 111456789
      Mtn[image]%M(1)%isTall%bool = .FALSE.
      Mtn[image]%M(2)%name = 'Tallest Mountain Is Everest'
      Mtn[image]%M(2)%id = 222456789
      Mtn[image]%M(2)%isTall%bool = .TRUE.
      Mtn[image]%M(3)%name = 'Second Tallest Mountain Is K-2'
      Mtn[image]%M(3)%id = 333456789
      Mtn[image]%M(3)%isTall%bool = .TRUE.

      print *,image,":",Mtn%M
      print *,image,":",Mtn[R_a(1)%rank]%M
      print *,image,":",Mtn[R_p%rank]%M
      print *,image,":",Mtn[R_t%rank]%M

      allocate(Mtn[image]%M_a(3))
      Mtn[image]%M_a(1)%name = 'Mountain Range Himalaya'
      Mtn[image]%M_a(1)%id = 111456111
      Mtn[image]%M_a(1)%isTall%bool = .FALSE.
      Mtn[image]%M_a(2)%name = 'Tallest Mountain Is Everest'
      Mtn[image]%M_a(2)%id = 222456222
      Mtn[image]%M_a(2)%isTall%bool = .TRUE.
      Mtn[image]%M_a(3)%name = 'Second Tallest Mountain Is K-2'
      Mtn[image]%M_a(3)%id = 333456333
      Mtn[image]%M_a(3)%isTall%bool = .TRUE.
      print *,image,":",Mtn[image]%M_a

      allocate(Mtn[image]%M_p(3))
      Mtn[image]%M_p(1)%name = 'Mountain Range Himalaya'
      Mtn[image]%M_p(1)%id = 111111111
      Mtn[image]%M_p(1)%isTall%bool = .FALSE.
      Mtn[image]%M_p(2)%name = 'Tallest Mountain Is Everest'
      Mtn[image]%M_p(2)%id = 222222222
      Mtn[image]%M_p(2)%isTall%bool = .TRUE.
      Mtn[image]%M_p(3)%name = 'Second Tallest Mountain Is K-2'
      Mtn[image]%M_p(3)%id = 333333333
      Mtn[image]%M_p(3)%isTall%bool = .TRUE.
      print *,image,":",Mtn[image]%M_p

      deallocate(Mtn[image]%M_a)
      deallocate(Mtn[image]%M_p)
   end if

end program main
