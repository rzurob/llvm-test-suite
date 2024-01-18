! GB DTP extension using:
! ftcx_dtp -qk -qreuse=base /tstdev/F2003/generic/syntax/C459/genericC459Assignment002.f
! opt variations: -qck -qnok -qreuse=none

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
!*  SECONDARY FUNCTIONS TESTED : with Assignment( = )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C459: define generic TB with same generic name with different access-spec
!*                                     within the same derived type
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

   type :: base(k1,n1)    ! (4,3)
      integer, kind :: k1
      integer, len  :: n1
      character(n1) :: c
      contains
         generic, private :: assignment(=) => typetotype
         procedure, pass, private :: typetotype => btob
   end type

   type, extends(base) :: child    ! (4,3)
      integer(k1) :: i
      contains
         procedure, pass :: typetotype => ctoc
   end type

   contains

   subroutine btob ( a , b )
      class(base(4,*)) :: a, b
      intent(out) :: a
      intent(in) :: b

      a%c = b%c

   end subroutine

   subroutine ctoc ( a , b )
      class(child(4,*)) :: a
      class(base(4,*)) :: b
      intent(out) :: a
      intent(in) :: b

      a%c = b%c

   end subroutine

end module

module n
   use m, only: child

   type, extends(child) :: gen3    ! (4,3)
      integer(k1) j
      contains
         generic :: assignment(=) => gtog
         procedure, pass :: gtog => gtog
   end type

   contains

   subroutine gtog ( a , b )
      class(gen3(4,*)) :: a, b
      intent(out) :: a
      intent(in) :: b

      a%c = b%c
      a%i = b%i
      a%j = b%j

   end subroutine

end module

program genericC459Assignment002
end program
