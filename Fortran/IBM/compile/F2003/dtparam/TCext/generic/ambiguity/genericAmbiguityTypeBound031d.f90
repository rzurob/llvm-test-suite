! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound031d.f
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
!*  DESCRIPTION                : Optional dummy args, with different name args, but position cannot distinguish
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

module genericName

   type b1(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, nopass :: threeargs1
         generic :: threeargs => threeargs1
   end type

   type, extends(b1) :: c1    ! (20,4)
      contains
         procedure, nopass :: threeargs2
         generic :: threeargs => threeargs2
   end type


   contains

      subroutine threeargs1(x,y,z)
         real, optional, intent(in) :: x
         integer, intent(in) :: y
         real, intent(out) :: z

         print *, 'threeargs1'

      end subroutine

      subroutine threeargs2(x,z,y)
         integer, optional, intent(in) :: x
         integer, intent(in) :: z
         real, intent(out) :: y

         print *, 'threeargs2'

      end subroutine

end module

program genericAmbiguityTypeBound031d
end program
