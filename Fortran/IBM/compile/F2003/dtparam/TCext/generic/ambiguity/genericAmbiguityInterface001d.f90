! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityInterface001d.f
! opt variations: -qnol

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
!*  DESCRIPTION                : two ambiguous interfaces defined in different scopes and bring into the same scope
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

module type

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
   end type

end module


module m
   use type, only: base

   interface operator(+)
      module procedure add1
   end interface

   contains

      integer(4) function add1(a, b)
         type(base(*,4)), intent(in) :: a
         integer(4), intent(in) :: b

         add1 = a%i + b

      end function

end module

module n
   use type, only: base

   interface operator(+)
      module procedure add2
   end interface

   contains

      integer(4) function add2(a, b)
         type(base(*,4)), intent(in) :: a
         integer(4), intent(in) :: b

         add2 = a%i + b

      end function

end module

program genericAmbiguityInterface001d
   use m
   use n

end program
