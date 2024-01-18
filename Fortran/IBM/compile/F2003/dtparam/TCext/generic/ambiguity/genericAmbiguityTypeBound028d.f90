! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound028d.f
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
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : With class hierarchy
!*                                 - distinguishable by position, but not by arg with names, with pass dummy arg with derived type dummy args
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
         generic :: fourargs => fourargs1
   end type

   type, extends(b1) :: c1(k2,n2)    ! (20,4,4,20)
       integer, kind :: k2
       integer, len  :: n2
      contains
         procedure, pass(dtv) :: fourargs2
         generic :: fourargs => fourargs2
   end type

   contains

      subroutine fourargs1(w,x,dtv,y,z)
         type(b1(*,4)), intent(in) :: w, y
         type(c1(*,4,4,*)), intent(in) :: x, z
         class(b1(*,4)), intent(in) :: dtv

         print *, 'fourargs1'

      end subroutine

      subroutine fourargs2(x,w,z,y,dtv)
         type(b1(*,4)), intent(in) :: x, y
         class(c1(*,4,4,*)), intent(in) :: w, z
         class(c1(*,4,4,*)), intent(in) :: dtv  !<- pass-object cannot distinguish the two procedures

         print *, 'fourargs2'

      end subroutine

end module

program genericAmbiguityTypeBound028d
end program
