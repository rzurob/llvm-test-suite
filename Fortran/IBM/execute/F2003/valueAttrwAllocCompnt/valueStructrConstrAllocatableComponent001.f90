!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with derived allocatable components, pass
!*                                         structure component as actual arg (including parent), try structure constructor being the actual arg
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

program valueStructrConstrAllocatableComponent001
   use m

   type(base) b1
   type(child) c1

   allocatable :: c1

   call foo ( base(inner(100)) )
   print *,'==='
   call bar ( inner(200) )
   print *,'==='
   call boo ( child(inner(300), inner(400)))
   print *,'==='

end program
