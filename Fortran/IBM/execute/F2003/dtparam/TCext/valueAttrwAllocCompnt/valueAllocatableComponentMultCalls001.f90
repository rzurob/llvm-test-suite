! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/valueAllocatableComponentMultCalls001.f
! opt variations: -ql -qreuse=none

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
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - contains multiple calls with and without value attribute
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

   type base(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i1
      integer(k1), pointer     :: i2
   end type

   integer :: g1

   interface
      subroutine foo2 ( a )
         import base
         type(base(4)) :: a
      end subroutine
   end interface

   contains

   subroutine foo1 ( a )
      type(base(4)), value  :: a

      print *, 'foo1:',a%i1, a%i2

      a = base(4)(100, a%i2)

      call foo2 ( a )

      print *, 'end foo1:',a%i1, a%i2

   end subroutine

end module

subroutine foo2 ( a )
   use m, only: base
   type(base(4)) :: a
   interface
      subroutine foo3 ( a )
         import base
         type(base(4)), value :: a
      end subroutine
   end interface

   print *, 'foo2:', a%i1, a%i2

   a%i1 = 2000
   a%i2 = 3000

   call foo3( a )

   print *, 'end foo2:', a%i1, a%i2

end subroutine


subroutine foo3 ( a )
   use m, only: base
   type(base(4)), value :: a

   print *, 'foo3:', a%i1, a%i2

   a%i1 = 40000
   a%i2 = 50000

   print *, 'end foo3:', a%i1, a%i2

end subroutine

program valueAllocatableComponentMultCalls001
   use m

   type(base(4)) :: b1
   class(base(4)), allocatable :: b2

   integer, pointer :: i1

   allocate ( i1, source = 99_4 )

   b1 = base(4)(10,i1 )
   print *, 'start:', b1%i1, b1%i2, i1
   call foo1(b1)
   print *, 'end:', b1%i1, b1%i2, i1
   
   i1=0
   
   allocate ( b2, source = base(4)(50,i1) )
   print *, 'start:', b2%i1, b2%i2, i1
   call foo1(b2)
   print *, 'end:', b2%i1, b2%i2, i1

end program
