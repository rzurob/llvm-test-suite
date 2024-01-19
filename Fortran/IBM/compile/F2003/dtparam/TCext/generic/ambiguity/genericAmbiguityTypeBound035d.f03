! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound035d.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : both are dummy procs and are type compatible
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
         procedure, nopass :: oneargs1
         generic :: oneargs => oneargs1
   end type

   type, extends(b1) :: c1    ! (20,4)
      contains
         procedure, nopass :: oneargs2
         generic :: oneargs => oneargs2
   end type

   contains

      subroutine oneargs1(y)

         abstract interface
            class(b1(:,4)) function myinterface1()
               import b1
               allocatable :: myinterface1
            end function
         end interface

         procedure(myinterface1) :: y

      end subroutine

      subroutine oneargs2(z)

         abstract interface
            class(b1(:,4)) function myinterface()
               import b1
               allocatable :: myinterface
            end function
         end interface

         procedure(myinterface) :: z

      end subroutine

end module

module genericName1

   type b11(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: i
      contains
         procedure, nopass :: oneargs3
         generic :: oneargs => oneargs3
         procedure, nopass :: oneargs4
         generic :: oneargs => oneargs4
   end type

   type, extends(b11) :: c11    ! (20,4)
   end type

   contains

      subroutine oneargs3(y)

         abstract interface
            class(b11(:,4)) function myinterface1()
               import b11
               allocatable :: myinterface1
            end function
         end interface

         procedure(myinterface1) :: y

      end subroutine

      subroutine oneargs4(z)

         abstract interface
            class(c11(:,4)) function myinterface()
               import c11
               allocatable :: myinterface
            end function
         end interface

         procedure(myinterface) :: z

      end subroutine

end module

program genericAmbiguityTypeBound035d
end program
