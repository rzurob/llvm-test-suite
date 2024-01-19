! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound043.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : both are dummy procedures and one is subroutine the other is function
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
         procedure, pass(a) :: twoargs1
         generic :: twoargs => twoargs1
   end type

   type, extends(b1) :: c1    ! (20,4)
      contains
         procedure, pass(a) :: twoargs2
         generic :: twoargs => twoargs2
   end type

   abstract interface
      subroutine firstsub(a, b)
         integer, intent(in) :: a, b
      end subroutine
   end interface

   abstract interface
      type(b1(20,4)) function secondfunc(a)
         import b1
         type(b1(*,4)), intent(in) :: a
      end function
   end interface

   contains

      subroutine twoargs1(a,b)
         class(b1(*,4)), intent(in) :: a
         procedure(firstsub) :: b

         print *, 'twoargs1'
         call b(a%i+20, 30)

      end subroutine

      subroutine twoargs2(a,b)
         class(c1(*,4)), intent(in) :: a
         procedure(secondfunc) :: b

         print *, 'twoargs2:', b( b1(20,4)(a%i) )

      end subroutine

end module

module subs
   use genericName, only: b1, c1
   contains

      subroutine twoints(a, b)
         integer, intent(in) :: a, b

         print *, 'twoints:',a,b

      end subroutine

      type(b1(20,4)) function oneb1(a)
         type(b1(*,4)), intent(in) :: a

         oneb1 = b1(20,4)( a%i + 10 )

      end function

end module

program genericAmbiguityTypeBound043
   use subs

   type(c1(20,4)) :: c1_1 = c1(20,4)(100)
   type(b1(20,4)) :: b1_1 = b1(20,4)(200)

   call c1_1%twoargs(twoints)
   call c1_1%twoargs(oneb1)

   call b1_1%twoargs(twoints)

end program
