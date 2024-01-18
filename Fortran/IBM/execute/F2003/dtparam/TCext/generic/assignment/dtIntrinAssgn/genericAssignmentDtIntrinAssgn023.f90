! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp -qreuse=base /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn023.f
! opt variations: -qnok -qnol -qdeferredlp -qreuse=none

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
!*                                 - for allocatable component
!*                                    - use defined assignment if the "declared"
!*                                      type of the component has a type-bound
!*                                      defined assignment consistent with the components
!*                                        - component is unlimited polymorphic
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
      class(*), allocatable :: u1
   end type

   type base(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: i
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k2) :: j
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b

         error stop 1_4

      end subroutine

end module

program genericAssignmentDtIntrinAssgn023
   use m

   type(container(4,20)) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3 )

   c1 = container(4,20) ( 10_8 )
   c2 = c1
   c3 = c2

   select type ( g => c1%u1 )
      type is ( integer(8) )
         print *, g
   end select

   select type ( g => c2%u1 )
      type is ( integer(8) )
         print *, g
   end select

   select type ( g => c3%u1 )
      type is ( integer(8) )
         print *, g
   end select

   c2 = container(4,20)(base(20,4)(20))
   c1 = c2
   c3 = c1

   select type ( g => c1%u1 )
      type is ( base(*,4) )
         print *, g
   end select

   select type ( g => c2%u1 )
      type is ( base(*,4) )
         print *, g
   end select

   select type ( g => c3%u1 )
      type is ( base(*,4) )
         print *, g
   end select

   c3 = container(4,20)(child(20,4)(30,40))
   c1 = c3
   c2 = c3

   select type ( g => c1%u1 )
      type is ( child(*,4) )
         print *, g
   end select

   select type ( g => c2%u1 )
      type is ( child(*,4) )
         print *, g
   end select

   select type ( g => c3%u1 )
      type is ( child(*,4) )
         print *, g
   end select

end program
