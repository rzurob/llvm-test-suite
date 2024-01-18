! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn014.f
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
!*                                      - container type contains a derived type
!*                                        component that has UD assignment of elemental and different ranks
!*                                        but it should only resolve to the elemental type bound
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
      integer, kind :: k1
      integer(k1)      i
      contains
         procedure :: itoi0
         generic :: assignment(=) => itoi0
   end type

   type container(k2)    ! (4)
      integer, kind   :: k2
      type(inner(k2)) :: inn0
      type(inner(k2)) :: inn1(3)
      type(inner(k2)) :: inn2(2,2)
      type(inner(k2)) :: inn3(2,2,2)
   end type

   contains

      elemental subroutine itoi0( a, b )
         class(inner(4)), intent(out) :: a
         class(inner(4)), intent(in) :: b

         a%i = b%i + 1

      end subroutine

end module

module n
   use m, only: container, inner

   interface assignment(=)
      module procedure itoi1
      module procedure itoi2
      subroutine itoi3( a, b )
         import inner
         class(inner(4)), intent(out) :: a(:,:,:)
         class(inner(4)), intent(in) :: b(:,:,:)
      end subroutine
   end interface

   contains

       subroutine itoi1( a, b )
         class(inner(4)), intent(out) :: a(:)
         class(inner(4)), intent(in) :: b(:)

         a%i = b%i

      end subroutine

      subroutine itoi2( a, b )
         class(inner(4)), intent(out) :: a(:,:)
         class(inner(4)), intent(in) :: b(:,:)

         a%i = b%i

      end subroutine

 end module

subroutine itoi3( a, b )
   use m, only: inner
   class(inner(4)), intent(out) :: a(:,:,:)
   class(inner(4)), intent(in) :: b(:,:,:)

   a%i = b%i

end subroutine

program genericAssignmentDtIntrinAssgn014
   use n

   type(container(4)) :: c1, c2

   allocatable :: c2

   c1 = container(4)( inner(4)(1), (/ inner(4)(2), inner(4)(3), inner(4)(4) /), reshape( source = (/ inner(4)(5), inner(4)(6), inner(4)(7), inner(4)(8)  /), shape =(/2,2/) ), &
        & reshape( source = (/ inner(4)(9), inner(4)(10), inner(4)(11), inner(4)(12), inner(4)(13), inner(4)(14), inner(4)(15), inner(4)(16)  /), shape =(/2,2,2/) ) )

   print *, c1
   allocate ( c2 )

   c2 = c1
   print *, c2

   c1 = c2
   print *, c1

   c2%inn0 = c1%inn0
   c2%inn1 = c1%inn1
   c2%inn2 = c1%inn2
   c2%inn3 = c1%inn3

   print *, c2

end program
