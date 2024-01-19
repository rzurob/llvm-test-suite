! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=self -qreuse=base /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn031.f
! opt variations: -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - polymorphic container with different types of type components
!*
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
   end type

   type container(k2,n2)    ! (4,20)
      integer, kind     :: k2
      integer, len      :: n2
      type(base(n2,k2)) :: b1
   end type

   type, extends(container) :: c_container    ! (4,20)
      type(child(n2,k2))  :: c1
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in)  :: b

         a%i = b%i

         select type ( a )
            type is ( child(*,4) )
               select type ( b )
                  type is ( child(*,4) )
                     a%j = b%j
               end select
         end select

         print *, 'bassgn'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn031
   use m

   class(container(4,20)), allocatable :: c1, c2
   class(c_container(4,20)), allocatable :: c3

   allocate ( c1, c2, c3 )

   select type ( g => c1 )
      type is ( container(4,*) )
         g = container(4,20)(base(20,4)(100))
         print *, g

         select type ( c2 )
            type is ( container(4,*) )
               c2 = c1
               print *, c2
         end select

         g%b1%i = 101

   end select

   select type ( c2 )
      type is ( container(4,*) )
         c2 = c1
         print *, c2
   end select

   deallocate ( c1, c2 )

   allocate ( c_container(4,20) :: c2, c1 )

   select type ( c2 )
      type is ( c_container(4,*) )

         c2 = c_container(4,20)( base(20,4)(200), child(20,4)(300, 400) )
         print *, c2

         select type ( c1 )
            type is ( c_container(4,*) )
               c1 = c2
               print *, c1
         end select

         select type ( c3 )
            type is ( c_container(4,*) )
               c3 = c2
               print *, c3
         end select

   end select

end program
