! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound036d.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : both are dummy procs and are type compatible, and different number and types of arguments
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

      subroutine oneargs1(y, x)
         integer, intent(in) :: x

         abstract interface
            class(b1(4)) function myinterface1(a,b)
               import b1
               allocatable :: myinterface1
               integer :: a, b
            end function
         end interface

         procedure(myinterface1) :: y

      end subroutine

      subroutine oneargs2(z, x)
         integer, intent(in) :: x

         abstract interface
            class(b1(4)) function myinterface(x,y)
               import b1
               allocatable :: myinterface
               real :: x,y
            end function
         end interface

         procedure(myinterface) :: z

      end subroutine

end module

module genericName1

   type b11(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: i
      contains
         procedure, nopass :: oneargs3
         generic :: oneargs => oneargs3
         procedure, nopass :: oneargs4
         generic :: oneargs => oneargs4
   end type

   type, extends(b11) :: c11    ! (4)
   end type

   contains

      subroutine oneargs3(y)

         abstract interface
            class(b11(4)) function myinterface1(i)
               import b11
               allocatable :: myinterface1
               integer :: i
            end function
         end interface

         procedure(myinterface1) :: y

      end subroutine

      subroutine oneargs4(z)

         abstract interface
            class(c11(4)) function myinterface(x,y)
               import c11
               allocatable :: myinterface
               real :: x,y
            end function
         end interface

         procedure(myinterface) :: z

      end subroutine

end module

program genericAmbiguityTypeBound036d
end program
