! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/valueAttrwAllocCompnt/valueAllocatableComponentInnerProcedure002.f
! opt variations: -qnok -ql

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
!*                                 - type: derived type with unlimited polymorphic allocatable components
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

   type inner(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i
   end type
   
   type base(k2)    ! (4)
      integer, kind :: k2
      class(*), allocatable :: i1
      contains
         procedure, nopass :: foo
   end type

   contains

   subroutine foo ( a )
      type(base(4)), value :: a

      select type ( g => a%i1 )
         type is ( integer )
            print *, g
         type is ( inner(4) )
            print *, g%i
      end select
      
      a = innerfoo ( a, a )
      
      select type ( g => a%i1 )
         type is ( integer )
            print *, g
         type is ( inner(4) )
            print *, g%i
      end select

      contains

         type(base(4)) function innerfoo( ia, ib )
            type(base(4)), value :: ia
            type(base(4)) :: ib

            ia = base(4) ( 1000000 )
            print *, 'innerfoo'
            
            select type ( g => ia%i1 )
               type is ( integer )
                  print *, g
               type is ( inner(4) )
                  print *, g%i
            end select
            
            select type ( g => ib%i1 )
               type is ( integer )
                  print *, g
               type is ( inner(4) )
                  print *, g%i
            end select

            ib = ia
            select type ( g => ia%i1 )
               type is ( integer )
                  print *, g
               type is ( inner(4) )
                  print *, g%i
            end select
            
            select type ( g => ib%i1 )
               type is ( integer )
                  print *, g
               type is ( inner(4) )
                  print *, g%i
            end select

            select type ( g => a%i1 )
               type is ( integer )
                  print *, g
               type is ( inner(4) )
                  print *, g%i
            end select
 
            innerfoo = base(4)( 100 - 200 + 400 )
            
         end function

   end subroutine

end module

program valueAllocatableComponentInnerProcedure002
   use m

   implicit type(base(4)) (a-z)
   allocatable :: p2

   p1 = base(4)(50)

   allocate ( p2, source = base(4)(inner(4)(200)) )

   call foo ( p1 )
   call foo ( p2 )

end program
