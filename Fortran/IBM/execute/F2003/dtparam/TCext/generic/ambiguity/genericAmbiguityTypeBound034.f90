! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound034.f
! opt variations: -qnok -qnol -qnodeferredlp

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

   type b1(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, nopass :: oneargs1
         generic :: oneargs => oneargs1
   end type

   type, extends(b1) :: c1(k2,n2)    ! (20,4,4,20)
       integer, kind :: k2
       integer, len  :: n2
      contains
         procedure, nopass :: oneargs2
         generic :: oneargs => oneargs2
   end type

   contains

      subroutine oneargs1(y)
         class(b1(*,4)), intent(in) :: y

         print *, 'oneargs1:', y%i

      end subroutine

      subroutine oneargs2(z)

         abstract interface
            class(b1(:,4)) function myinterface()
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

class(b1(:,4)) function foo()
   use genericName, only: b1
   allocatable :: foo
   allocate ( foo, source = b1(20,4)(1000) )
end function

program genericAmbiguityTypeBound034
   use genericName

   type(c1(20,4,4,20)) :: c1_1
   type(b1(20,4)) :: b1_1 = b1(20,4)(100)

   interface
      class(b1(:,4)) function foo()
         import b1
         allocatable :: foo
      end function
   end interface

   call c1_1%oneargs(b1(20,4)(10))
   call c1_1%oneargs(b1_1)
   call c1_1%oneargs(foo)

end program
