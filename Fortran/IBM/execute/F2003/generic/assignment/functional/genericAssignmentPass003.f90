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
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: pass-obj specified assignment to different derived types
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

   type base
      integer(4) :: i = -999
      contains
         procedure, pass(b) :: base1_base
         procedure, pass(a) :: base_base1
         generic :: assignment(=) => base1_base, base_base1
   end type

   type base1
      integer(8) :: k = -999
   end type

   contains

   subroutine base1_base ( a, b )
      class(base1), intent(out) :: a
      class(base), intent(in)   :: b

      a%k = INT(b%i,8)

      print *,'base1_base'

   end subroutine

   subroutine base_base1 ( a, b )
      class(base), intent(out) :: a
      class(base1), intent(in) :: b

      a%i = INT(b%k,4)

      print *,'base_base1'

   end subroutine


end module

program genericAssignmentPass003
   use m

   type(base) :: b1
   type(base1) :: bb1
   
   class(base), allocatable :: b2
   class(base1), pointer :: bb2

   b1 = base1(100)
   print *, b1

   bb1 = base(200)
   print *, bb1

   allocate ( b2, bb2 )
   
   b2 = bb1
   print *, b2%i
   
   bb2 = b1
   print *, bb2%k   

end program
