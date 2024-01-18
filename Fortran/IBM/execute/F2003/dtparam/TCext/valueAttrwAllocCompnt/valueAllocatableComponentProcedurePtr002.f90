! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/valueAllocatableComponentProcedurePtr002.f
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
!*                                 - type: derived type with derived type allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - with procedure pointer
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
      integer(k1), allocatable :: i1
   end type

   type base(k2)    ! (4)
      integer, kind                 :: k2
      class(inner(k2)), allocatable :: in1
   end type

   contains

   subroutine foo1 ( a )
      type(base(4)), value  :: a

      print *, 'foo1:',a%in1%i1
      a = base(4)(inner(4)(100))
      print *, 'end foo1:',a%in1%i1

   end subroutine

   subroutine foo2 ( a )
      type(base(4)), value  :: a

      print *, 'foo2:',a%in1%i1

      a = base(4)(inner(4)(200))
      print *, 'end foo2:',a%in1%i1

   end subroutine

end module

program valueAllocatableComponentProcedurePtr002
   use m

   abstract interface
      subroutine xxx(a )
         import base
         type(base(4)), value :: a
      end subroutine
   end interface

   procedure(xxx), pointer :: p1

   type(base(4)), allocatable :: b1
   allocate ( b1, source = base(4)(inner(4)(999)) )

   p1 => foo1

   print *, 'start', b1%in1%i1
   call p1(b1)
   print *, 'after foo1', b1%in1%i1

   p1 => foo2

   print *, b1%in1%i1
   call p1(b1)
   print *, 'after foo2', b1%in1%i1

end program
