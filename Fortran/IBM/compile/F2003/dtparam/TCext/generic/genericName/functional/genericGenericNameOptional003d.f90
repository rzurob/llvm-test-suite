! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/generic/genericName/functional/genericGenericNameOptional003d.f
! opt variations: -qnok -qnol

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
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: optional dummy arguments and ambiguity
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

   type :: base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      contains
         procedure, pass :: optional1
         procedure, pass :: optional2
         generic :: print => optional1, optional2
   end type

   contains

      subroutine optional1 ( a ,b )
         class(base(4,*)), intent(in) :: a, b

      end subroutine

      subroutine optional2 ( a, b )
         class(base(4,*)), intent(in) :: a
         class(base(4,*)), intent(in), optional :: b

      end subroutine

end module

program genericGenericNameOptional003d
end program
