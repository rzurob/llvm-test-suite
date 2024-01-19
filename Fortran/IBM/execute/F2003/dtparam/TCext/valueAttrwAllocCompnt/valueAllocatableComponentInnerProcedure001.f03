! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/valueAttrwAllocCompnt/valueAllocatableComponentInnerProcedure001.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with intrinsic allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - with inner procedure
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
      contains
         procedure, nopass :: foo
   end type

   contains

   subroutine foo ( a )
      type(base(4)), value :: a

      print *, a%i1
      a = innerfoo ( a, a )
      print *, a%i1

      contains

         type(base(4)) function innerfoo( ia, ib )
            type(base(4)), value :: ia
            type(base(4)) :: ib

            ia = base(4) ( 1000000 )
            print *, 'innerfoo', ia%i1, ib%i1
            ib = ia
            print *, ia%i1, ib%i1

            print *, a%i1

            innerfoo = base(4)( ia%i1 + ib%i1 )

         end function

   end subroutine

end module

program valueAllocatableComponentInnerProcedure001
   use m

   implicit type(base(4)) (a-z)
   allocatable :: p2

   p1 = base(4)(50)

   allocate ( p2, source = base(4)(200) )

   call foo ( p1 )

   call foo ( p2 )

end program
