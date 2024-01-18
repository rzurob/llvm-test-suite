!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectProcedureDT05.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-05-20
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
!*  Testing Nested derived type. One level of nesting is used.
!*  Type-bound procedure.
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   type bool
      logical bool
   end type bool

   type mountain(nl)
      integer, kind :: nl
      character(len=nl) :: name
      integer :: id
      integer :: dept_id
      type(bool) :: isTallest
      contains
      procedure, pass :: compare=>compare_mountain
   end type mountain

   contains

   logical function compare_mountain(M1, M2)
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
      else if (M1%isTallest%bool .NEQV. M2%isTallest%bool) then
         print *,M1%isTallest%bool," .NEQV. ",M2%isTallest%bool
         ERROR STOP 104
      end if

      compare_mountain = .TRUE.
   end function compare_mountain

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
   M1%isTallest%bool = .FALSE.
   M2%name = 'Tallest Mountain Is Everest'
   M2%id = 222456789
   M2%dept_id = 301 + image
   M2%isTallest%bool = .TRUE.
   M3%name = 'Second Tallest Mountain Is K-2'
   M3%id = 333456789
   M3%dept_id = 302 + image
   M3%isTallest%bool = .FALSE.

   if (image .EQ. 1) then
      print *,image,":M1:",M1
      print *,image,":M2:",M2
      print *,image,":M3:",M3
      M[1] = M1
      print *,"M == M1 ",M1%compare(M[1])
   else if (image .EQ. 2) then
      M[2] = M2
      print *,"M == M2 ",M2%compare(M[2])
   else
      M[image] = M3
      print *,"M == M3 ",M3%compare(M[image])
   end if

   print *,image,":M :",M

end program main
