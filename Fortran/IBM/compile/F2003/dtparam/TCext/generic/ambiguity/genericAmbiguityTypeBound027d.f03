! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound027d.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : With class hierarchy
!*                                 - distinguishable by position, but not by arg with names, with pass dummy arg
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
         procedure, pass(dtv) :: fourargs1
         procedure, nopass :: fourargs2
         generic :: fourargs => fourargs1, fourargs2
   end type

   type, extends(b1) :: c1    ! (20,4)
   end type

   contains

      subroutine fourargs1(w,x,dtv,y,z)
         type(b1(*,4)), intent(in) :: w, y
         type(c1(*,4)), intent(in) :: x, z
         class(b1(*,4)), intent(in) :: dtv

         print *, 'fourargs1'

      end subroutine

      subroutine fourargs2(x,w,z,y)
         type(b1(*,4)), intent(in) :: x, y
         class(c1(*,4)), intent(in) :: w, z

         print *, 'fourargs2'

      end subroutine

end module

program genericAmbiguityTypeBound027d
end program