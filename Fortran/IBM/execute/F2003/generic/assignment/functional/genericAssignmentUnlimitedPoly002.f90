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
!*  DESCRIPTION                : assignment: 2nd operand is unlimited polymorphic
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

   type :: mytype
      character(3) :: c
   end type

   type :: base
      class(*), allocatable :: i
      contains
         procedure, pass :: bagnmt
         generic :: assignment(=) => bagnmt
   end type

   contains

      subroutine bagnmt ( a, b )
         class(base), intent(out) :: a
         class(*), intent(in) :: b

         if ( allocated(a%i) ) deallocate ( a%i )
         allocate ( a%i , source = b )

         print *, 'basgmt'

      end subroutine

end module

program genericAssignmentUnlimitedPoly002
   use m

   class(base), allocatable :: b1
  type(base) :: b2
   class(*), pointer :: u1

   allocate ( b1 )

   allocate ( u1, source = 1234 )

   b1 = u1

   select type ( h => b1%i )
      type is ( integer )
         print *,h
   end select

   allocate ( u1, source = 'ibmftn' )

   b2 = u1

   select type ( h => b2%i )
      type is ( character(*) )
         print *,h
   end select

   allocate ( u1, source = 101.101 )

   b1 = u1

   select type ( h => b1%i )
      type is ( real )
         print *,h
   end select

   allocate ( u1, source = mytype('xlf') )

   b2 = u1

   select type ( h => u1 )
      type is ( mytype )
         print *, h
   end select

end program
