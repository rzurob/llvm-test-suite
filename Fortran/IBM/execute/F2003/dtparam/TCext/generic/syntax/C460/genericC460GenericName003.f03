! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/generic/syntax/C460/genericC460GenericName003.f
! opt variations: -qnok -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : C460: specific binding exists in parent type
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
      integer(k1)      i
      contains
         procedure, pass :: mypass
         procedure, nopass :: mynopass => mypass
   end type

   type, extends(base) :: child(k2,n2)    ! (20,4,4,20)
       integer, kind :: k2
       integer, len  :: n2
      contains
         generic :: donothing => mypass, mynopass
   end type

   contains

      subroutine mypass (a)
         class(base(*,4)), intent(in) :: a
      end subroutine

end module

program genericC460GenericName003
end program
