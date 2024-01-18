! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/ambiguity/genericAmbiguityInterface010d.f
! opt variations: -qnol -qnodeferredlp

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
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : ambiguous but between interface and deferred type bound
!*
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

module binoperator

   type, abstract :: base1(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure(addb), pass, deferred :: adda
         generic :: operator(*) => adda
   end type

   interface operator(*)
      class(base1(:,4)) function addb(a, b)
         import base1
         class(base1(*,4)), intent(in) :: a
         class(base1(*,4)), intent(in)  :: b
         allocatable :: addb
      end function
   end interface

   type, extends(base1) :: child1    ! (20,4)
      contains
         procedure, pass :: adda
   end type

   contains

      class(base1(:,4)) function adda(a, b)
         class(child1(*,4)), intent(in) :: a
         class(base1(*,4)), intent(in)  :: b
         allocatable :: adda

         allocate ( child1(20,4) :: adda )
      end function

end module

class(base1(:,4)) function addb(a, b)
   use binoperator, only: base1, child1

   class(base1(*,4)), intent(in) :: a
   class(base1(*,4)), intent(in)  :: b
   allocatable :: addb

   allocate ( child1(20,4) :: addb )

end function

program genericAmbiguityInterface010d
end program

