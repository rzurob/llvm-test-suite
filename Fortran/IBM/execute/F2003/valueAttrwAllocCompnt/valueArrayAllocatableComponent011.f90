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
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
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
      integer, allocatable :: i(:)
   end type

   type(base) :: b1

   contains

      subroutine deallocActual()

         print *, 'deallocating component of b1', b1%i,lbound(b1%i,1), ubound(b1%i,1), allocated(b1%i)

         deallocate ( b1%i )

         print *, allocated(b1%i)

      end subroutine

      subroutine foo(a)
         type(base), value :: a

         print *, 'inside foo', a%i, lbound(a%i,1), ubound(a%i,1), allocated(a%i)
         call deallocActual()
         print *, 'after deallocActual', a%i, lbound(a%i,1), ubound(a%i,1), allocated(a%i)

      end subroutine

end module

program valueArrayAllocatableComponent011
   use m

   b1 = base(null())

   allocate ( b1%i(11:20), source = (/ (100+i, i=1,10) /) )

   print *, 'deallocating component of b1', b1%i,lbound(b1%i,1), ubound(b1%i,1), allocated(b1%i)
   call foo ( b1 )
   print *, 'main:', allocated(b1%i)

end program
