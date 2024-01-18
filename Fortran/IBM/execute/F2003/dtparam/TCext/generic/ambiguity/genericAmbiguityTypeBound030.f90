! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound030.f
! opt variations: -ql

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
!*  DESCRIPTION                : Optional dummy args, and some class hierarchy
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

   type b1(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, nopass :: threeargs1
         generic :: threeargs => threeargs1
   end type

   type, extends(b1) :: c1    ! (4)
      contains
         procedure, nopass :: threeargs2
         generic :: threeargs => threeargs2
   end type

   type, extends(b1) :: c2    ! (4)
   end type

   type, extends(c1) :: g1    ! (4)
   end type

   contains

      subroutine threeargs1(x,y,z)
         class(c1(4)), intent(in) :: x,y,z

         print *, 'threeargs1'

      end subroutine

      subroutine threeargs2(x,z,w)
         class(b1(4)), intent(in) :: x
         class(g1(4)) :: z
         class(c2(4)), optional :: w

         print *, 'threeargs2'

      end subroutine

end module

program genericAmbiguityTypeBound030
   use genericName

   type(b1(4)) :: b1_1
   type(c1(4)) :: c1_1
   type(g1(4)) :: g1_1
   type(c2(4)) :: c2_1

   call b1_1%threeargs( c1_1, g1_1, c1_1 )

   call c1_1%threeargs( c1_1, g1_1, c1_1 )

   call c1_1%threeargs( c1_1, g1_1, c2_1 )

   call c1_1%threeargs( c1_1, g1_1 )
   
   call g1_1%threeargs( c1_1, g1_1, c1_1 )

   call g1_1%threeargs( c1_1, g1_1, c2_1 )

   call g1_1%threeargs( c1_1, g1_1 )

end program
