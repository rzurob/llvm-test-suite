!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                  - Perform generic type bound assignment for the type
!*                                    component when it is declared for the type
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type com1
      integer :: i
      contains
         generic :: assignment(=) => c1assgn
         procedure, pass :: c1assgn
   end type

   type com2
      character(3) :: c
      contains
         generic :: assignment(=) => c2assgn
         procedure, pass :: c2assgn
   end type

   contains

      subroutine c1assgn ( a, b )
         class(com1), intent(out) :: a
         class(com1), intent(in) :: b

         a%i = b%i
         print *, 'c1assgn'

      end subroutine

      subroutine c2assgn ( a, b )
         class(com2), intent(out) :: a
         class(com2), intent(in) :: b

         a%c = b%c
         print *, 'c2assgn'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn001
   use m

   type base
      integer :: x
      type(com1) :: c1
      type(com2) :: c2
   end type

   type(base) :: b1
   type(base), allocatable :: b2
   type(base), pointer :: b3
   
   type(com2), parameter :: c2 = com2('ftn')
   
   b1 = base( 10, com1(20), com2('ibm') )
   print *, b1
   
   allocate ( b2, b3 )
   
   b2 = base( 30, com1(40), c2 )
   print *, b2
   
   b3 = b1
   print *, b3
   
   b2 = b3
   print *, b2

end program
