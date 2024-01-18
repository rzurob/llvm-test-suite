!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
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

   type b1
      integer :: i
      contains
         procedure, nopass :: oneargs1
         generic :: oneargs => oneargs1
   end type

   type, extends(b1) :: c1
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

   type(c1) :: c1_1
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
