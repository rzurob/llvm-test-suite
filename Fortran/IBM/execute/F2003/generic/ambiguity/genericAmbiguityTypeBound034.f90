!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : one is object dummy arg, the other is dummy procedure of derived type
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
         class(b1), intent(in) :: y

         print *, 'oneargs1:', y%i

      end subroutine

      subroutine oneargs2(z)

         abstract interface
            class(b1) function myinterface()
               import b1
               allocatable :: myinterface
            end function
         end interface

         procedure(myinterface) :: z

         associate ( ggg => z() )
            print *, 'oneargs2:', ggg%i
         end associate

      end subroutine

end module

class(b1) function foo()
   use genericName, only: b1
   allocatable :: foo
   allocate ( foo, source = b1(1000) )
end function

program genericAmbiguityTypeBound034
   use genericName

   type(c1) :: c1_1
   type(b1) :: b1_1 = b1(100)

   interface
      class(b1) function foo()
         import b1
         allocatable :: foo
      end function
   end interface

   call c1_1%oneargs(b1(10))
   call c1_1%oneargs(b1_1)
   call c1_1%oneargs(foo)

end program
