! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound029.f
! opt variations: -qnol -qnodeferredlp

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
!*  DESCRIPTION                : With no class hierarchy
!*                                 - distinguishable dummy args are now placed before name distinguished dummys
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

   type, extends(b1) :: c1    ! (20,4)
      contains
         procedure, pass(dtv) :: fourargs2
         generic :: fourargs => fourargs2
   end type

   contains

      subroutine fourargs1(y,z,w,x,dtv)
         type(b1(*,4)), intent(in) :: w, y
         type(c1(*,4)), intent(in) :: x, z
         class(b1(*,4)), intent(in) :: dtv

         print *, 'fourargs1'

      end subroutine

      subroutine fourargs2(z,y,x,w,dtv)
         type(b1(*,4)), intent(in) :: x, y
         class(c1(*,4)), intent(in) :: w, z
         class(c1(*,4)), intent(in) :: dtv  !<- pass-object cannot distinguish the two procedures

         print *, 'fourargs2'

      end subroutine

end module

program genericAmbiguityTypeBound029
   use genericname

   class(c1(:,4)), allocatable :: c1_2
   type(b1(20,4)) :: b1_2 = b1(20,4)(10)
   type(c1(20,4)) :: c1_1 = c1(20,4)(100)

   allocate ( c1(20,4) :: c1_2 )

   call c1_2%fourargs( c1_1, b1_2, x=b1_2, w=c1_1 )
   call c1_2%fourargs( b1_2, c1_1, x=c1_2, w=b1_2 )

end program
