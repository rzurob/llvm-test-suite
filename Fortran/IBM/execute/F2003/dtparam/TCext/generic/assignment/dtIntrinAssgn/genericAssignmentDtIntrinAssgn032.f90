! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn032.f
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
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - polymorphic container with different types of type components of allocatables
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j
   end type

   type container(k2)    ! (4)
      integer, kind                 :: k2
      type(base(k2)), allocatable     :: b1
   end type

   type, extends(container) :: c_container    ! (4)
      type(child(k2)), allocatable :: c1
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in)  :: b

         a%i = b%i

         select type ( a )
            type is ( child(4) )
               select type ( b )
                  type is ( child(4) )
                     a%j = b%j
               end select
         end select

         print *, 'bassgn'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn032
   use m

   class(container(4)), allocatable :: c1, c2
   class(c_container(4)), allocatable :: c3

   allocate ( c1, c2, c3 )

   select type ( g => c1 )
      type is ( container(4) )
         g = container(4)(base(4)(100))
         print *, g%b1

         select type ( c2 )
            type is ( container(4) )
               c2 = g
               print *, c2%b1
         end select

         g%b1%i = 101

   end select

   select type ( c2 )
      type is ( container(4) )
         c2 = c1
         print *, c2%b1
   end select

   deallocate ( c1, c2 )

   allocate ( c_container(4) :: c2, c1 )

   select type ( c2 )
      type is ( c_container(4) )

         c2 = c_container(4)( base(4)(200), child(4)(300, 400) )
         print *, c2%b1, c2%c1

         select type ( c1 )
            type is ( c_container(4) )
               c1 = c2
               print *, c1%b1, c1%c1
         end select

         select type ( c3 )
            type is ( c_container(4) )
               c3 = c2
               print *, c3%b1, c3%c1
         end select

   end select

end program
