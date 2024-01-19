! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound040d.f
! opt variations: -qnok -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : ambiguous when optional arg is added, without optional no ambiguity
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

   type, extends(b1) :: c1(k2,n2)    ! (20,4,4,20)
       integer, kind :: k2
       integer, len  :: n2
      contains
         procedure, nopass :: threeargs2
         generic :: threeargs => threeargs2
   end type

   type, extends(b1) :: c2(k3,n3)    ! (20,4,4,20)
       integer, kind :: k3
       integer, len  :: n3
   end type

   contains

      subroutine threeargs1(x, y, z)
         class(b1(*,4)), intent(out) :: x
         class(c1(*,4,4,*)), intent(out) :: y
         class(b1(*,4)), optional, intent(out) :: z

         print *, 'threeargs1'

      end subroutine

      subroutine threeargs2(x, y, z)
         class(c2(*,4,4,*)), intent(out) :: x
         class(c1(*,4,4,*)), intent(out) :: y
         class(c2(*,4,4,*)), intent(out) :: z

         print *, 'threeargs2'

      end subroutine

end module

program genericAmbiguityTypeBound040d
end program
