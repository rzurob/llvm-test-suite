! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/syntax/C1112/genericC1112Assignment001d.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : C1112: generic-spec in use only statement shall not specify
!*                                      a generic binding
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
         procedure, pass :: assign
         procedure, pass :: assignint
         generic :: assignment =>assign, assignint
   end type

   contains

      subroutine assign(a,b)
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b

         a%i = b%i

      end subroutine

      subroutine assignint(a,b)
         class(base(*,4)), intent(out) :: a
         integer, intent(in) :: b

         a%i = b

      end subroutine
end module

program genericC1112Assignment001d
   use m, only: assignment(=)
end program
