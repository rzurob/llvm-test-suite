!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectAssignmentDT06.f
!*
!*  DATE                       : 2011-03-07
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
!*  Testing Nested derived type. Two levels of nesting is used.
!*  Assignment using derived type.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   type whatRank
      integer rank
   end type whatRank

   type isTall
      logical bool
      type(whatRank) :: whatRank
   end type isTall

   type mountain(nl)
      integer, kind :: nl
      character(len=nl) :: name
      integer :: id
      integer :: dept_id
      type(isTall) :: isTall
      contains
      procedure :: compare
   end type mountain

   contains

   subroutine compare(M1, M2)
      class (mountain(30)), intent(in) :: M1
      class (mountain(30)), intent(in) :: M2

      if (M1%name .NE. M2%name) then
         print *,M1%name," .NE. ",M2%name
         ERROR STOP 101
      else if (M1%id .NE. M2%id) then
         print *,M1%id," .NE. ",M2%id
         ERROR STOP 102
      else if (M1%dept_id .NE. M2%dept_id) then
         print *,M1%dept_id," .NE. ",M2%dept_id
         ERROR STOP 103
      else if (M1%isTall%bool .NEQV. M2%isTall%bool) then
         print *,M1%isTall%bool," .NEQV. ",M2%isTall%bool
         ERROR STOP 104
      else if (M1%isTall%whatRank%rank .NE. M2%isTall%whatRank%rank) then
         print *,M1%isTall%whatRank%rank," .NE. ",M2%isTall%whatRank%rank
         ERROR STOP 104
      end if
   end subroutine compare

end module DT

program main

   use DT

   implicit none

   integer image
   type(mountain(30)) :: M1
   type(mountain(30)) :: M2
   type(mountain(30)) :: M3
   type(mountain(30)), save :: M[*]

   image = this_image()

   M1%name = 'Mountain Range Himalaya'
   M1%id = 111456789
   M1%dept_id = 300 + image
   M1%isTall%bool = .FALSE.
   M1%isTall%whatRank%rank = 0
   M2%name = 'Tallest Mountain Is Everest'
   M2%id = 222456789
   M2%dept_id = 301 + image
   M2%isTall%bool = .TRUE.
   M2%isTall%whatRank%rank = 1
   M3%name = 'Second Tallest Mountain Is K-2'
   M3%id = 333456789
   M3%dept_id = 302 + image
   M3%isTall%bool = .TRUE.
   M3%isTall%whatRank%rank = 2

   if (image .EQ. 1) then
      print *,image,":M1:",M1
      print *,image,":M2:",M2
      print *,image,":M3:",M3
      M[1] = M1
      call M[1]%compare(M1)
   else if (image .EQ. 2) then
      M[2] = M2
      call M[2]%compare(M2)
   else
      M[image] = M3
      call M[image]%compare(M3)
   end if

   print *,image,":M :",M

end program main
