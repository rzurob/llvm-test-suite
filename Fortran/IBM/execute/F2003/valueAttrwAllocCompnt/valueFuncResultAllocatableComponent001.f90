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
!*                                 - type: derived type with derived allocatable components, pass
!*                                         structure component as actual arg (including parent), test function results
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

         print *, 'inside foo1', a%inb%i

         a%inb%i = -999

         print *, 'after foo', allocated(a%inb), a%inb%i

      end subroutine

      subroutine bar(a)
         type(inner), value :: a

         print *, 'inside bar', a%i
         deallocate ( a%i )
         print *, 'after bar', allocated(a%i)

      end subroutine

end module

type(base) function getbase ()
   use m, only:base, inner
   getbase = base(inner(100))
end function

class(base) function getpolychild(a)
   use m, only:base, inner, child
   allocatable :: getpolychild

   class(base) :: a
   optional :: a

   if ( .not. present(a) ) then
      allocate ( getpolychild, source = child(inner(200), inner(2000)) )
   else
      allocate ( getpolychild, source=(a) )
   end if

end function

class(base) function getpolydummy(a)
   use m, only : base, inner, child

   type(child), value :: a
   allocatable :: getpolydummy

   allocate ( getpolydummy, source = a )

end function

program valueFuncResultAllocatableComponent001
   use m

   interface
      type(base) function getbase ()
         import base, inner
      end function

      class(base) function getpolychild(a)
         import base, inner, child
         allocatable :: getpolychild

         class(base) :: a
         optional :: a
      end function

      class(base) function getpolydummy(a)
         import  base, inner, child
         type(child), value :: a
         allocatable :: getpolydummy
      end function
   end interface

   call foo(getbase())
   print *, "======"
   call foo(getpolychild())
   print *, "======"
   call foo(getpolydummy(child(inner(10),inner(20))))

   associate ( gg => getbase() )
      print *, "======"

      call foo(getpolychild(gg))
      print *, gg%inb%i

   end associate

end program
