! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/misc/genericMisc005d.f
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
!*  SECONDARY FUNCTIONS TESTED : misc.
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : parent type does not exist in the extend statement
!*                               previously ICE (315242)
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
  type, extends(whereisit) :: base(n1,k1)    ! (20,4)
     integer, kind :: k1
     integer, len  :: n1
     integer(k1)   :: i
  end type

end module

program genericMisc005
end program
