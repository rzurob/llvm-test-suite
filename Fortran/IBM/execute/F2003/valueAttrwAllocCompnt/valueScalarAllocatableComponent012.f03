!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with intrinsic allocatable components, pass
!*                                         structure component as actual arg (including parent)
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

   type inner
      integer, allocatable :: i
   end type

   type base
      type(inner), allocatable :: inb
   end type

   type, extends(base) :: child
      type(inner), allocatable :: inc
   end type

   contains

      subroutine foo(a)
         type(base), value :: a

         print *, 'inside foo', a%inb%i

         call bar(a%inb)

         print *, allocated(a%inb%i)
         deallocate ( a%inb%i )
         print *, allocated(a%inb%i)
         deallocate ( a%inb )
         print *, 'after foo', allocated(a%inb)

      end subroutine

      subroutine bar(a)
         type(inner), value :: a

         print *, 'inside bar', a%i
         deallocate ( a%i )
         print *, 'after bar', allocated(a%i)

      end subroutine

      subroutine boo(a)
         class(base) :: a

         print *, 'inside boo', a%inb%i

         call foo(a)

         print *, 'inside boo 2', a%inb%i

         a%inb = inner(500)

         print *, 'after boo', allocated(a%inb)

      end subroutine

end module

program valueScalarAllocatableComponent012
   use m

   type(base) b1
   type(child) c1

   allocatable :: c1

   b1 = base(inner(100))

   call foo ( b1 )

   print *, '=========='
   print *, 'main b1', allocated ( b1%inb%i ), allocated ( b1%inb ), b1%inb%i
   print *, '=========='

   allocate ( c1 )

   c1 = child( inner(200), inner(300) )

   call foo ( c1%base )
   print *, '=========='
   print *, 'main c1', allocated ( c1%inb%i ), allocated ( c1%inb ), c1%inb%i
   print *, '=========='

   call bar ( c1%base%inb )
   print *, '=========='
   print *, 'main c1%inb', allocated ( c1%inb%i ), allocated ( c1%inb ), c1%inb%i
   print *, '=========='

   call bar ( c1%inc )
   print *, '=========='
   print *, 'main c1%inc', allocated ( c1%inc%i ), allocated ( c1%inc ), c1%inc%i
   print *, '=========='

   call boo ( c1 )
   print *, '=========='
   print *, 'main c1', allocated ( c1%inb%i ), allocated ( c1%inb ), c1%inb%i
   print *, '=========='

end program