! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueArrayAllocatableComponent005.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: unlimited polymorphic scalar allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
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
      real(k1), allocatable :: j
   end type

   type base(k2)    ! (4)
      integer, kind :: k2
      class(*), allocatable :: u1(:)
      integer(k2)   :: i1
   end type

   type(base(4)), pointer :: b3

   contains

      subroutine foo( dtv )
         type(base(4)), value :: dtv

         select type ( g => dtv%u1 )
            type is ( integer )
               print *, g
            type is ( real )
               print *, g
            type is ( inner(4) )
               do kk = lbound ( g, 1 ), ubound ( g, 1 )
                  print *, g(kk)%i
               end do
            type is ( cinner(4) )
               do kk = lbound ( g, 1 ), ubound ( g, 1 )
                  print *, g(kk)%i, g(kk)%j
               end do
         end select

         print *, dtv%i1

         dtv = base(4)( (/ -999 /), -999 )

         select type ( g => dtv%u1 )
            type is ( integer )
               print *, g
         end select

         print *, dtv%i1

      end subroutine

end module

program valueArrayAllocatableComponent005
   use m

   type(base(4)) :: b1
   type(base(4)), allocatable :: b2

   b1 = base(4)( (/ 10, 11, 12/) , 20 )

   call foo ( b1 )

   select type ( g => b1%u1 )
      type is ( integer )
         print *, g
      type is ( real )
         print *, g
      type is ( inner(4) )
         do kk = lbound ( g, 1 ), ubound ( g, 1 )
            print *, g(kk)%i
         end do
      type is ( cinner(4) )
         do kk = lbound ( g, 1 ), ubound ( g, 1 )
            print *, g(kk)%i, g(kk)%j
         end do
   end select

   print *,  b1%i1

   allocate ( b2, source = base(4) ( (/ 100.0, 101.0, 102.0 /) , 200 ) )

   call foo ( b2 )

   select type ( g => b2%u1 )
      type is ( integer )
         print *, g
      type is ( real )
         print *, g
      type is ( inner(4) )
         do kk = lbound ( g, 1 ), ubound ( g, 1 )
            print *, g(kk)%i
         end do
      type is ( cinner(4) )
         do kk = lbound ( g, 1 ), ubound ( g, 1 )
            print *, g(kk)%i, g(kk)%j
         end do
   end select

   print *,  b2%i1

   allocate ( b3, source = base(4) ( (/ inner(4)(1000), inner(4)(1001), inner(4)(1002), inner(4)(1003) /), 2000 ) )

   call foo ( b3 )

   select type ( g => b3%u1 )
      type is ( integer )
         print *, g
      type is ( real )
         print *, g
      type is ( inner(4) )
         do kk = lbound ( g, 1 ), ubound ( g, 1 )
            print *, g(kk)%i
         end do
      type is ( cinner(4) )
         do kk = lbound ( g, 1 ), ubound ( g, 1 )
            print *, g(kk)%i, g(kk)%j
         end do
   end select

   print *,  b3%i1

    b1 = base(4)( (/ cinner(4)(100, 200.00) /) , 30 )

   call foo ( b1 )

   select type ( g => b1%u1 )
      type is ( integer )
         print *, g
      type is ( real )
         print *, g
      type is ( inner(4) )
         do kk = lbound ( g, 1 ), ubound ( g, 1 )
            print *, g(kk)%i
         end do
      type is ( cinner(4) )
         do kk = lbound ( g, 1 ), ubound ( g, 1 )
            print *, g(kk)%i, g(kk)%j
         end do
   end select

   print *,  b1%i1

end program