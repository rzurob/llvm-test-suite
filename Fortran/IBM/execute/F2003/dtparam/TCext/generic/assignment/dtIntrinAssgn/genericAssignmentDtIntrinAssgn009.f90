! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn009.f
! opt variations: -qnok -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                  - A derived-type intrinsic assignment is performed as if each component of variable
!*                                    were assigned from the corresponding component of expr using pointer
!*                                    assignment for each pointer component
!*                                      - try a derived type containing an unlimited polymorphic pointer and have UD assignment
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

   type container(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      class(*), pointer :: u => null()
   end type

   type inner(n2,k2)    ! (20,4)
      integer, kind        :: k2
      integer, len         :: n2
      integer(k2), pointer :: j => null()
   end type

   interface assignment!(=)
      module procedure uassgn
   end interface

   contains

      subroutine uassgn ( a, b )
         class(*), pointer, intent(out) :: a
         class(*), pointer, intent(in)  :: b

         error stop 1_4

      end subroutine

end module

program genericAssignmentDtIntrinAssgn009
   use m

   type(container(4,20)) :: c1, c2

   c1 = container(4,20)()
   allocate ( c1%u, source = 10_4 )

   c2 = c1
   select type ( g => c2%u )
      type is ( integer )
         print *, g, associated ( c1%u, g )
   end select

   allocate ( c1%u, source = inner(20,4)() )
   select type ( h => c1%u )
      type is ( inner(*,4) )
         allocate ( h%j, source = 20_4 )
   end select

   c2 = c1
   select type ( g => c2%u )
      type is ( inner(*,4) )
         print *, g%j, associated ( c1%u, g )
         select type ( j => c1%u )
            type is ( inner(*,4) )
               print *, associated ( j%j, g%j )
         end select
   end select

end program
