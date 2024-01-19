! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=none /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn012.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=self -qreuse=base

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                      - try a derived type containing an derived type scalar and array
!*                                        with elemental UD assignment in generic tb with class hierarchy
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

   type, extends(base) :: child(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j
   end type

   type container(k3,n3,n4)    ! (4,3,20)
      integer, kind            :: k3
      integer, len             :: n3,n4
      type(base(n4,k3))        :: b1
      type(child(n4,k3,n4,k3)) :: c1
      character(n3)            :: ccc
      type(base(n4,k3))        :: b2(3)
      type(child(n4,k3,n4,k3)) :: c2(2:4)
   end type

   contains

      elemental subroutine bassgn ( a, b )
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

      end subroutine

end module

program genericAssignmentDtIntrinAssgn012
   use m

   type(container(4,3,20)) :: c1, c2
   type(container(4,3,20)) :: c3

   pointer :: c2
   allocatable :: c3

   c1 = container(4,3,20)(base(20,4)(1), child(20,4,20,4)(2,3), 'abc', (/ base(20,4)(4), base(20,4)(5), base(20,4)(6) /), (/ child(20,4,20,4)(7,8), child(20,4,20,4)(9,10), child(20,4,20,4)(11,12) /) )

   allocate ( c2, c3 )

   c2 = c1
   c3 = c2

   print *, c1
   print *, c2
   print *, c3

end program
