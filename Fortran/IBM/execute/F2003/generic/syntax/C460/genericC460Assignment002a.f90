!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DESCRIPTION                : C460: specific-binding exist in parent type, generic assignment defined in gen3 type
!*                                     both base and child type should not call generic assignment
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
      integer i
      contains
         procedure, pass :: myassgn
   end type

   type, extends(base) :: emptychild
      contains
         procedure, pass :: myassgn => childassgn
   end type

   type, extends(emptychild) :: gen3
      integer j
      contains
         generic :: assignment( = ) => myassgn
   end type

   contains

      subroutine myassgn(a, b)
         class(base), intent(out) :: a
         class(base), intent(in)  :: b
         a%i = b%i

         print *, 'base'

      end subroutine

      subroutine childassgn(a, b)
         class(emptychild), intent(out) :: a
         class(base), intent(in)  :: b
         a%i = b%i

         print *, 'child'

      end subroutine

end module

program genericC460Assignment002a
   use m

   type(base)       :: b1, b2
   type(emptychild) :: c1, c2

   b1 = base(10)
   b2 = base(20)
   c1 = emptychild(30)
   c2 = emptychild(40)

   b1 = b2
   b2 = c1%base
   c1 = c2

   print *, b1, b2, c1, c2

end program
