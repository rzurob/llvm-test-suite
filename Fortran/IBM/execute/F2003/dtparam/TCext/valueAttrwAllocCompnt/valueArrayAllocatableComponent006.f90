! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueArrayAllocatableComponent006.f
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
!*                                 - type: polymorphic array allocatable components
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
      class(inner(k2)), allocatable :: u1(:)
   end type

   type(base(4)), pointer :: b1

   contains

      subroutine foo( dtv )
         type(base(4)), target, value :: dtv

         select type ( g => dtv%u1 )
            type is ( inner(4) )
               do i=lbound(g,1), ubound(g,1)
                  print *, g(i)%i
               end do
            type is ( cinner(4) )
               do i=lbound(g,1), ubound(g,1)
                  print *, g(i)%i, g(i)%j
               end do
         end select

         print *, associated ( b1, dtv )

         dtv = base(4) ( (/ inner(4)(-999) /))
         b1 => dtv

         print *, associated ( b1, dtv )

         select type ( g => b1%u1 )
            type is ( inner(4) )
               do i=lbound(g,1), ubound(g,1)
                  print *, g(i)%i
               end do
            type is ( cinner(4) )
               do i=lbound(g,1), ubound(g,1)
                  print *, g(i)%i, g(i)%j
               end do
         end select

         b1 = base(4) ( (/  cinner(4)(-9999,-9999) /) )

         print *, associated ( b1, dtv )

         select type ( g => b1%u1 )
            type is ( inner(4) )
               do i=lbound(g,1), ubound(g,1)
                  print *, g(i)%i
               end do
            type is ( cinner(4) )
               do i=lbound(g,1), ubound(g,1)
                  print *, g(i)%i, g(i)%j
               end do
         end select

         select type ( g => dtv%u1 )
            type is ( inner(4) )
               do i=lbound(g,1), ubound(g,1)
                  print *, g(i)%i
               end do
            type is ( cinner(4) )
               do i=lbound(g,1), ubound(g,1)
                  print *, g(i)%i, g(i)%j
               end do
         end select

      end subroutine

end module

program valueScalarAllocatableComponent006
   use m

   allocate ( b1, source = base(4) ( (/ inner(4)(100), inner(4)(200), inner(4)(300), inner(4)(400) /) ) )
   call foo ( b1 )

   ! b1 is undefined

   allocate ( b1, source = base(4) ( (/ cinner(4)( 1001, 2001 ), cinner(4)( 1002, 2002 ), cinner(4)( 1003, 2003 ), cinner(4)( 1004, 2004 ), cinner(4)( 1005, 2005 ) /) ) )
   call foo ( b1 )

   ! b1 is undefined

end program
