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
!*  DESCRIPTION                : assignment: pass-obj specified with different types ( sequence type as well)
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
         procedure, pass(b) :: sbase_base
         generic :: assignment(=) => sbase_base, base_sbase
         procedure, pass :: base_sbase
   end type

   type, extends(base) :: child
      contains
         procedure, pass(b) :: sbase_base => sbase_child
         procedure, pass :: base_sbase => child_sbase
   end type

   type :: sbase
      sequence
      integer(4) :: i = 999
   end type

   contains

   subroutine sbase_base ( a, b )
      type(sbase), intent(out) :: a
      class(base), intent(in)  :: b

      a%i = b%i

      print *,'sbase_base'

   end subroutine

   subroutine base_sbase ( a, b )
      class(base), intent(out) :: a
      type(sbase), intent(in)  :: b

      a%i = b%i

      print *,'base_sbase'

   end subroutine

   subroutine sbase_child ( a, b )
      type(sbase), intent(out) :: a
      class(child), intent(in)  :: b

      a%i = b%i

      print *,'sbase_child'

   end subroutine

   subroutine child_sbase ( a, b )
      class(child), intent(out) :: a
      type(sbase), intent(in)  :: b

      a%i = b%i

      print *,'child_sbase'

   end subroutine


end module

program genericAssignmentPass015
   use m

   class(base), allocatable :: b1
   class(child), pointer :: c1

   type(sbase) :: s1, s2

   allocate( b1, c1 )

   s1 = sbase(1000)
   b1 = s1
   print *, b1%i

   s2 = sbase(2000)
   c1 = s2
   print *, c1%i

   deallocate ( b1, c1 )

   allocate ( b1, source = base(50))
   allocate ( c1, source = child(500))

   s1 = b1
   s2 = c1

   print *, s1
   print *, s2

   deallocate ( b1 )
   allocate ( b1, source = child(5000))

   s1 = b1
   b1 = s2
   print *,s1
   print *, b1%i

   b1 = sbase(1)
   s1 = base(2)

   print *, b1%i, s1%i

   c1 = sbase(3)
   s2 = child(4)

   print *, c1%i, s2%i

end program
