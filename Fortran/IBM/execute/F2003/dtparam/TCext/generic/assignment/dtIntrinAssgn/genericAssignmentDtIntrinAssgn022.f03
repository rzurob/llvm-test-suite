! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn022.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self -qreuse=none

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
!*                                        - base type has a defined assignment and child type overrides it
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
      integer(k1)   :: i = -999
      contains
         procedure :: btob
         generic :: assignment(=) => btob
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j = -999
      contains
         procedure :: btob => ctob
   end type

   type container(k2)    ! (4)
      integer, kind                :: k2
      class(base(k2)), allocatable :: b1
      type(child(k2)), allocatable :: c1
   end type

   contains

      subroutine btob(a,b)
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in) :: b

         a%i = b%i + 1

         print *, 'btob'

      end subroutine

      subroutine ctob(a,b)
         class(child(4)), intent(out) :: a
         class(base(4)), intent(in) :: b

         select type ( b )
            type is ( base(4) )
               a%base = b
            type is ( child(4) )
               a%base = b%base ! calling the generic type bound
               a%j = b%j + 1
         end select

         print *, 'ctob'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn022
   use m

   type(container(4)) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3
   target :: c1

   c1 = container(4)( base(4)(100), child(4)(200,300) )
   print *, c1%b1%i, c1%c1

   allocate ( c2, c3 )
   c2 = c1
   print *, c2%b1%i, c2%c1

   c3 = c2
   print *, c3%b1%i, c3%c1

   c2 = container(4) ( child(4)(1000,2000), child(4)(3000, 4000) )

   select type ( g => c2%b1 )
      type is ( child(4) )
         print *, g, c2%c1
   end select

   c1 = c2

   select type ( g => c1%b1 )
      type is ( child(4) )
         print *, g, c1%c1
   end select

   c3 = c1

   select type ( g => c3%b1 )
      type is ( child(4) )
         print *, g, c3%c1
   end select

end program
