! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar010d.f
! opt variations: -qnol -qreuse=none

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
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             ambiguous generic-name generic bindings
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
         procedure, nopass :: printbase
         generic :: print => printbase
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
      contains
         procedure, nopass :: printchild
         generic :: print => printchild
   end type

   contains

      subroutine printbase()
         print *, 'printbase'
      end subroutine
      
      subroutine printchild()
         print *, 'printchild'
      end subroutine

end module

program genericGenericNameScalar010d
end program
