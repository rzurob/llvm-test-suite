! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueScalarAllocatableComponent006.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: polymorphic scalar allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - try the pointer association status with pointers in module and dummy-arg has target attribute
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

   type, extends(inner) :: cinner    ! (4)
      integer(k1), allocatable :: j
   end type

   type base(k2)    ! (4)
      integer, kind                 :: k2
      class(inner(k2)), allocatable :: u1
   end type

   type(base(4)), pointer :: b1

   contains

      subroutine foo( dtv )
         type(base(4)), target, value :: dtv

         select type ( g => dtv%u1 )
            type is ( inner(4) )
               print *, g%i
            type is ( cinner(4) )
               print *, g%i, g%j
         end select

         print *, associated ( b1, dtv )

         dtv = base(4) ( inner(4)(-999) )
         b1 => dtv

         print *, associated ( b1, dtv )

         select type ( g => b1%u1 )
            type is ( inner(4) )
               print *, g%i
            type is ( cinner(4) )
               print *, g%i, g%j
         end select

         b1 = base(4) ( cinner(4)(-9999,-9999) )

         print *, associated ( b1, dtv )

         select type ( g => b1%u1 )
            type is ( inner(4) )
               print *, g%i
            type is ( cinner(4) )
               print *, g%i, g%j
         end select

         select type ( g => dtv%u1 )
            type is ( inner(4) )
               print *, g%i
            type is ( cinner(4) )
               print *, g%i, g%j
         end select

      end subroutine

end module

program valueScalarAllocatableComponent006
   use m

   allocate ( b1, source = base(4) (inner(4)(100)) )
   call foo ( b1 )

   ! b1 is undefined

   allocate ( b1, source = base(4) (cinner(4) (1000, 2000 ) ) )

   call foo ( b1 )

   ! b1 is undefined

end program
