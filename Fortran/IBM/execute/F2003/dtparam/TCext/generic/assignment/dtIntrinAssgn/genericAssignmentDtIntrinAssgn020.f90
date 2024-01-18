! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn020.f
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
!*                                 - for allocatable component
!*                                    - use defined assignment if the "declared"
!*                                      type of the component has a type-bound
!*                                      defined assignment consistent with the components
!*                                        - base type has a defined assignment
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
   end type

   type container(k2)    ! (4)
      integer, kind                :: k2
      type(base(k2)), allocatable  :: b1
      type(child(k2)), allocatable :: c1
   end type

   contains

      subroutine btob(a,b)
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in) :: b

         a%i = b%i + 1
         
         select type ( a )
            type is ( child(4) )
               select type ( b )
                  type is ( child(4) )
                     a%j = b%j + 1                  
               end select
         end select
         print *, 'btob'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn020
   use m

   type(container(4)) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3
   target :: c1

   c1 = container(4)( base(4)(100), child(4)(200,300) )
   print *, c1%b1, c1%c1

   allocate ( c2, c3 )
   c2 = c1
   print *, c2%b1, c2%c1

   c3 = c2
   print *, c3%b1, c3%c1

   c3 = container(4)( base(4)(50), child(4)(51, 52) )
   print *, c3%b1, c3%c1

   c2 => c1
   c2 = c3

   print *, c1%b1, c1%c1
   print *, c2%b1, c2%c1

   c1%b1%i = -999

   c1%b1 = c2%c1%base
   print *, c1%b1, c1%c1

end program
