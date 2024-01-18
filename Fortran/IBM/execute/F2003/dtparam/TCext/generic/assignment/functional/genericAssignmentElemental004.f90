! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/generic/assignment/functional/genericAssignmentElemental004.f
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
!*  DESCRIPTION                : assignment: non-polymorphic elemental assignment for type component as well as type
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
      integer(k1)   :: i
      contains
         procedure, pass :: iassgn
         generic :: assignment(=) => iassgn
   end type

   type base(k2)    ! (4)
      integer, kind   :: k2
      type(inner(k2)) :: in(3)
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   contains

      elemental subroutine iassgn ( a, b )
         class(inner(4)), intent(inout) :: a
         type(inner(4)), intent(in) :: b

         a%i = b%i + 1

      end subroutine

      elemental subroutine bassgn ( a, b )
         class(base(4)), intent(inout) :: a
         type(base(4)), intent(in) :: b

         a%in = b%in

      end subroutine

end module

program genericAssignmentElemental004
   use m

   type(base(4)) :: b1
   type(base(4)) :: b2(3)
   type(base(4)), allocatable :: b3(:)
   
   b1 = base(4)(inner(4)(100))
   print *, b1
   
   b2 = base(4)(inner(4)(200))
   print *, b2
   
   allocate ( b3(5) )
   b3 = base(4)(inner(4)(300))
   print *, b3
   
   b2 = (/ base(4)(inner(4)(2000)), base(4)(inner(4)(2001)), base(4)(inner(4)(2002)) /)
   print *, b2
   
   b3 = (/ b2, base(4)( (/inner(4)(2004), inner(4)(2005), inner(4)(2006)/) ), base(4)( (/inner(4)(2007), inner(4)(2008), inner(4)(2009)/) ) /)
   print *, b3

end program
