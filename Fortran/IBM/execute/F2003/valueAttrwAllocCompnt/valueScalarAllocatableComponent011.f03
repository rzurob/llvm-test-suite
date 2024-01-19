!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with intrinsic allocatable components
!*                                 - deallocatable of actual argument should not affect the value dummy arg
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
      integer, allocatable :: i
   end type

   type(base) :: b1

   contains

      subroutine deallocActual()

         print *, 'deallocating component of b1', b1%i

         deallocate ( b1%i )

         print *, allocated(b1%i)

      end subroutine

      subroutine foo(a)
         type(base), value :: a

         print *, 'inside foo', a%i, allocated(a%i)
         call deallocActual()
         print *, 'after deallocActual', a%i, allocated(a%i)

      end subroutine

end module

program valueScalarAllocatableComponent011
   use m

   b1 = base(10000)

   call foo(b1)

   print *, allocated(b1%i)

end program
