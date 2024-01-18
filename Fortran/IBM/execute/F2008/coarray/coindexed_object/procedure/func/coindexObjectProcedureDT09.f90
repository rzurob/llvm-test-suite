!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectProcedureDT09.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-05-27
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coindex object Procedure Call
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
!*  Uses corank of 1.
!*  Testing Nested derived type. Four levels of nesting is used.
!*  Procedure pointer.
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   type four
      complex nesting
   end type four

   type three
      real nesting
      type(four) :: num
   end type three

   type whatRank
      integer rank
      type(three) :: num
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
   end type mountain

   contains

   subroutine compare(M1, M2, img)
      class (mountain(30)), intent(in) :: M1
      class (mountain(30)), intent(in) :: M2
      integer, intent(in) :: img

      if (M1%name .NE. M2%name) then
         print *,img,": ",M1%name," .NE. ",M2%name
         ERROR STOP 101
      else if (M1%id .NE. M2%id) then
         print *,img,": ",M1%id," .NE. ",M2%id
         ERROR STOP 102
      else if (M1%dept_id .NE. M2%dept_id) then
         print *,img,": ",M1%dept_id," .NE. ",M2%dept_id
         ERROR STOP 103
      else if (M1%isTall%bool .NEQV. M2%isTall%bool) then
         print *,img,": ",M1%isTall%bool," .NEQV. ",M2%isTall%bool
         ERROR STOP 104
      else if (M1%isTall%whatRank%rank .NE. M2%isTall%whatRank%rank) then
         print *,img,": ",M1%isTall%whatRank%rank," .NE. ",M2%isTall%whatRank%rank
         ERROR STOP 105
      else if (M1%isTall%whatRank%num%nesting .NE. M2%isTall%whatRank%num%nesting) then
         print *,img,": ",M1%isTall%whatRank%num%nesting," .NE. ",M2%isTall%whatRank%num%nesting
         ERROR STOP 106
      else if (M1%isTall%whatRank%num%num%nesting .NE. M2%isTall%whatRank%num%num%nesting) then
         print *,img,": ",M1%isTall%whatRank%num%num%nesting," .NE. ",M2%isTall%whatRank%num%num%nesting
         ERROR STOP 107
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
   procedure(compare), pointer :: p_ptr => NULL()

   p_ptr => compare

   image = this_image()

   M1%name = 'Mountain Range Himalaya'
   M1%id = 111456789
   M1%dept_id = 300 + image
   M1%isTall%bool = .FALSE.
   M1%isTall%whatRank%rank = 0
   M1%isTall%whatRank%num%nesting = 3
   M1%isTall%whatRank%num%num%nesting = 4
   M2%name = 'Tallest Mountain Is Everest'
   M2%id = 222456789
   M2%dept_id = 301 + image
   M2%isTall%bool = .TRUE.
   M2%isTall%whatRank%rank = 1
   M2%isTall%whatRank%num%nesting = 3
   M2%isTall%whatRank%num%num%nesting = 4
   M3%name = 'Second Tallest Mountain Is K-2'
   M3%id = 333456789
   M3%dept_id = 302 + image
   M3%isTall%bool = .TRUE.
   M3%isTall%whatRank%rank = 2
   M3%isTall%whatRank%num%nesting = 3
   M3%isTall%whatRank%num%num%nesting = 4

   if (image .EQ. 1) then
      print *,image,":M1:",M1
      print *,image,":M2:",M2
      print *,image,":M3:",M3
      M[1] = M1
      call p_ptr(M1, M[1], 1)
   else if (image .EQ. 2) then
      M[2] = M2
      call p_ptr(M2, M[2], 2)
   else
      M[image] = M3
      call p_ptr(M3, M[image], image)
   end if

   print *,image,":M :",M

end program main
