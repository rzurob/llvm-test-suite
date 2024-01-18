! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound041d.f
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
!*  DESCRIPTION                : ambiguous when optional arg is added, without optional no ambiguity
!*                               with pass dummy args
!*
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
         procedure, pass(d) :: threeargs1
         generic :: threeargs => threeargs1
   end type

   type, extends(b1) :: c1    ! (20,4)
      contains
         procedure, pass(e) :: threeargs2
         generic :: threeargs => threeargs2
   end type

   type, extends(b1) :: c2    ! (20,4)
   end type

   contains

      subroutine threeargs1(a,b,c,d)
         class(b1(*,4)), intent(in), optional :: a

         class(c1(*,4)), intent(in) :: b
         class(c2(*,4)), intent(in) :: c

         class(b1(*,4)), intent(in) :: d

         print *, 'threeargs1'

      end subroutine

      subroutine threeargs2(b,e,c)
         class(b1(*,4)), intent(in) :: b
         class(b1(*,4)), intent(in) :: c

         class(c1(*,4)), intent(in) :: e

         print *, 'threeargs2'

      end subroutine

end module

program genericAmbiguityTypeBound041d

! the following call is ambiguous
! call c1_1%threeargs( b=c1_1, c=c2(10) )

end program
