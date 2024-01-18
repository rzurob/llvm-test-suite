!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectAssignmentDT12.f
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
!*  Uses corank of 1.
!*  Assignment using derived type with allocatable components.
!*  Copying/moving data among images. Works with only odd number of images.
!*  The extra or less number of images will be ignored.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   implicit none

   type :: Kid
      character :: name(30)
      integer :: age
   end type Kid

   type :: Person
      character, ALLOCATABLE :: name(:)
      integer :: age
      type(Kid), ALLOCATABLE :: kids(:)
      contains
      procedure :: print
   end type Person

   contains

   subroutine print(Per, image)
      class(Person), intent(in) :: Per
      integer, intent(in) :: image
      integer k

      print *,"Image:",image,": Name: ",Per%name," Age = ",Per%age,"Kids = ",size(Per%kids)
      do k = 1, size(Per%kids)
         print *,"   Kid",k,"'s Name: ",Per%kids(k)%name
         print *,"   Age:",Per%kids(k)%age
      end do
   end subroutine print

end module DT

program main

   use DT
   implicit none

   integer, PARAMETER :: SIZE = 5
   integer p, k, image
   type(Person), save :: persons(SIZE)[*]

   image = this_image()
   ! Assign to odd images
   if (mod(image, 2) .EQ. 1) then
      do p = 1, SIZE
         allocate(persons(p)[image]%name(20))
         persons(p)[image]%age = 2*p+40+image
         allocate(persons(p)[image]%kids(p+image))
         persons(p)[image]%name = (/ 'T','e','s','t','i','n','g',' ','P','e','r','s','o','n',' ',achar(p+48) /)
         do k = 1, p+image
            persons(p)[image]%kids(k)%name = reshape( (/ 'T','e','s','t','i','n','g',' ','K','i','d',' ',achar(k+48) /), shape(persons(p)[image]%kids(k)%name) )
            persons(p)[image]%kids(k)%age = k + persons(p)[image]%age / 2
         end do
      end do
      ! Print
      do p = 1, SIZE
         call persons(p)[image]%print(image)
         deallocate(persons(p)[image]%name)
         persons(p)[image]%age = 2*p+40+image
         deallocate(persons(p)[image]%kids)
      end do
   end if

   contains

end program main

