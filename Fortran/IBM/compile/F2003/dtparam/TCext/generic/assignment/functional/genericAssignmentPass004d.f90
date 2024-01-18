! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/assignment/functional/genericAssignmentPass004d.f
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
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: pass-obj specified assignment to different derived types
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
      integer(k1)   :: i = -999
      contains
         procedure, pass(b) :: base1_base
         generic :: assignment(=) => base1_base
   end type

   type base1(n2,k2)    ! (20,8)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: k = -999
      contains
         procedure :: base1_base_ambig
         generic :: assignment(=) => base1_base_ambig
   end type

   contains

      subroutine base1_base ( a, b )
         class(base1(*,8)), intent(out) :: a
         class(base(*,4)), intent(in)   :: b

         print *, 'base1_base'

      end subroutine

      subroutine base1_base_ambig ( a, b )
         class(base1(*,8)), intent(out) :: a
         class(base(*,4)), intent(in)   :: b

         print *, 'base1_base_ambig'

      end subroutine

end module

program genericAssignmentPass004d
end program
