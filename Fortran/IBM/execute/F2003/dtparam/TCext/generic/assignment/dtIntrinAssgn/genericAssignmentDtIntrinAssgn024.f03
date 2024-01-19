! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=none /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn024.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self -qreuse=base

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - for allocatable component
!*                                    - use defined assignment if the "declared"
!*                                      type of the component has a type-bound
!*                                      defined assignment consistent with the components
!*                                        - derived type does not have generic tb, but extended type does
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
         procedure, pass :: bassgn
   end type

   type, extends(base) :: child(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j
      contains
         generic :: assignment(=) => bassgn
   end type

   type container(k3,n3)    ! (4,20)
      integer, kind                        :: k3
      integer, len                         :: n3
      class(base(:,k3)), allocatable       :: b1
      class(child(:,k3,:,k3)), allocatable :: c1
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b

         a%i = b%i + 1

         select type ( a )
            type is ( child(*,4,*,4) )
               select type ( b )
                  type is ( child(*,4,*,4) )
                     a%j = b%j + 1
               end select
         end select

         print *, 'bassgn'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn024
   use m

   type(container(4,20)) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3 )

   c1 = container(4,20) ( base(20,4)(1), child(20,4,20,4)(1,2) )
   c2 = c1
   c3 = c2

   print *, c1%b1%i, c1%c1%i, c1%c1%j
   print *, c2%b1%i, c2%c1%i, c2%c1%j
   print *, c3%b1%i, c3%c1%i, c3%c1%j

   c2 = container(4,20) ( child(20,4,20,4)(1,1), child(20,4,20,4)(1,2) )
   c1 = c2
   c3 = c1

   select type ( g => c1%b1 )
      type is ( child(*,4,*,4) )
         print *, g, c1%c1%i, c1%c1%j
   end select

   select type ( g => c2%b1 )
      type is ( child(*,4,*,4) )
         print *, g, c2%c1%i, c2%c1%j
   end select

   select type ( g => c3%b1 )
      type is ( child(*,4,*,4) )
         print *, g, c3%c1%i, c3%c1%j
   end select

end program
