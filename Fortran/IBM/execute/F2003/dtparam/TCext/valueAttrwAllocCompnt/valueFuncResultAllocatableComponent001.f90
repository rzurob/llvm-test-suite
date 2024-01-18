! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueFuncResultAllocatableComponent001.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

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

   type inner(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i
   end type

   type base(k2)    ! (4)
      integer, kind                :: k2
      type(inner(k2)), allocatable :: inb
   end type

   type, extends(base) :: child    ! (4)
      type(inner(k2)), allocatable :: inc
   end type

   contains

      subroutine foo(a)
         type(base(4)), value :: a

         print *, 'inside foo', a%inb%i

         call bar(a%inb)

         print *, 'inside foo1', a%inb%i

         a%inb%i = -999

         print *, 'after foo', allocated(a%inb), a%inb%i

      end subroutine

      subroutine bar(a)
         type(inner(4)), value :: a

         print *, 'inside bar', a%i
         deallocate ( a%i )
         print *, 'after bar', allocated(a%i)

      end subroutine

end module

type(base(4)) function getbase ()
   use m, only:base, inner
   getbase = base(4)(inner(4)(100))
end function

class(base(4)) function getpolychild(a)
   use m, only:base, inner, child
   allocatable :: getpolychild

   class(base(4)) :: a
   optional :: a

   if ( .not. present(a) ) then
      allocate ( getpolychild, source = child(4)(inner(4)(200), inner(4)(2000)) )
   else
      allocate ( getpolychild, source=(a) )
   end if

end function

class(base(4)) function getpolydummy(a)
   use m, only : base, inner, child

   type(child(4)), value :: a
   allocatable :: getpolydummy

   allocate ( getpolydummy, source = a )

end function

program valueFuncResultAllocatableComponent001
   use m

   interface
      type(base(4)) function getbase ()
         import base, inner
      end function

      class(base(4)) function getpolychild(a)
         import base, inner, child
         allocatable :: getpolychild

         class(base(4)) :: a
         optional :: a
      end function

      class(base(4)) function getpolydummy(a)
         import  base, inner, child
         type(child(4)), value :: a
         allocatable :: getpolydummy
      end function
   end interface

   call foo(getbase())
   print *, "======"
   call foo(getpolychild())
   print *, "======"
   call foo(getpolydummy(child(4)(inner(4)(10),inner(4)(20))))

   associate ( gg => getbase() )
      print *, "======"

      call foo(getpolychild(gg))
      print *, gg%inb%i

   end associate

end program
