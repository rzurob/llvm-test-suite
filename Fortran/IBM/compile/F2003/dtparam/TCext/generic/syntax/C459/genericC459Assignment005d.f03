! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/generic/syntax/C459/genericC459Assignment005d.f
! opt variations: -qck -qnok -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment( = )
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

   type base(k1,n1)    ! (4,3)
      integer, kind :: k1
      integer, len  :: n1
      character(n1) :: c
      contains
         generic, private :: assignment(=) => ttob
         procedure :: ttob

   end type

   type, extends(base) :: child(k2,n2)    ! (4,3,4,20)
       integer, kind :: k2
       integer, len  :: n2
      contains
         generic :: assignment(=) => ttoc
         procedure :: ttoc
   end type

   contains

   subroutine ttob ( a , b )
      class(base(4,*)) :: a, b
      intent(out) :: a
      intent(in) :: b

      a%c = b%c

   end subroutine

   subroutine ttoc ( a , b )
      character(3) :: b
      class(child(4,*,4,*))  :: a
      intent(out)  :: a
      intent(in)   :: b

      a%c = b

   end subroutine

end module

program genericC459Assignment005d
end program
