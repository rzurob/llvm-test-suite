! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound032.f
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
!*  DESCRIPTION                : one is object dummy arg, the other is dummy procedure
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
         procedure, nopass :: oneargs1
         generic :: oneargs => oneargs1
   end type

   type, extends(b1) :: c1    ! (4)
      contains
         procedure, nopass :: oneargs2
         generic :: oneargs => oneargs2
   end type

   contains

      subroutine oneargs1(y)
         integer, intent(in) :: y

         print *, 'oneargs1:', y

      end subroutine

      subroutine oneargs2(z)

         abstract interface
            real function myinterface()
            end function
         end interface

         procedure(myinterface) :: z

         print *, 'oneargs2:', z()

      end subroutine

end module

real function foo()
   foo = 5.0
end function

program genericAmbiguityTypeBound032
   use genericName

   type(c1(4)) :: c1_1
   integer :: i

   abstract interface
      real function myinterface()
      end function
   end interface

   procedure(myinterface) :: foo

   i = foo()
   call c1_1%oneargs( i )
   call c1_1%oneargs( foo )
   call c1_1%oneargs( 10 )

end program
