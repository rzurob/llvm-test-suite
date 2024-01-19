!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: polymorphic array allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - try the pointer association status with polymorphic pointers in module and dummy-arg has target attribute
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

   type, extends(inner) :: cinner
      integer, allocatable :: j
   end type

   type base
      class(inner), allocatable :: u1(:)
   end type

   type, extends(base) :: child
      class(inner), allocatable :: u2(:)
   end type

   class(base), pointer :: b1

   contains

      subroutine foo( dtv )
         type(base), target, value :: dtv

         select type ( g => dtv%u1 )
            type is ( inner )
               do j=lbound(g,1), ubound(g,1)
                  print *, g(j)%i
               end do
            type is ( cinner )
               do j=lbound(g,1), ubound(g,1)
                  print *, g(j)%i, g(j)%j
               end do
         end select

         print *, associated ( b1, dtv )

         dtv = base ( (/ inner(-999) /) )
         b1 => dtv

         print *, associated ( b1, dtv )

         select type ( g => b1%u1 )
            type is ( inner )
               do i=lbound(g,1), ubound(g,1)
                  print *, g(i)%i
               end do
            type is ( cinner )
               do i=lbound(g,1), ubound(g,1)
                  print *, g(i)%i, g(i)%j
               end do
         end select

      end subroutine

end module

program valueArrayAllocatableComponent007
   use m

   allocate ( b1, source = base ( (/ inner(100), inner(200), inner(300) /) ) )
   call foo ( b1 )

   ! b1 is undefined

   allocate ( b1, source = base ( (/ cinner (1000, 2000 ), cinner (1001, 2001 ), cinner (1002, 2002 ) /) ) )

   call foo ( b1 )

   ! b1 is undefined

   allocate ( b1, source = child ( (/ inner(201), inner(202), inner(203), inner(204) /), (/ cinner(1111111,1111111) /) ) )
   call foo ( b1 )

   ! b1 is undefined

   allocate ( b1, source = child ( (/ cinner (3000, 4000), cinner (3001, 4001) /),(/  inner(111111) /) ) )

   call foo ( b1 )

   ! b1 is undefined

end program
