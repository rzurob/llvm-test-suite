! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar007.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic bindings with different kind type parameters of arguments
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m

   type base(k1)    ! (1)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, pass :: kind1
         procedure, pass :: kind2
         procedure, pass :: kind4
         procedure, pass :: kind8
         generic :: assgn => kind1, kind2, kind4, kind8
   end type

   contains

      subroutine kind1 (a, i)
         class(base(1)), intent(inout) :: a
         integer(1), intent(in) :: i

         a%i = i
         print *,'kind1'

      end subroutine

      subroutine kind2 (a, i)
         class(base(1)), intent(inout) :: a
         integer(2), intent(in) :: i

         call a%assgn(int(i,1))
         print *,'kind2'

      end subroutine

      subroutine kind4 (a, i)
         class(base(1)), intent(inout) :: a
         integer(4), intent(in) :: i

         call a%assgn(int(i,2))
         print *,'kind4'

      end subroutine

      subroutine kind8 (a, i)
         class(base(1)), intent(inout) :: a
         integer(8), intent(in) :: i

         call a%assgn(int(i,4))
         print *,'kind8'

      end subroutine

end module

program genericGenericNameScalar007
   use m

   type(base(1)) :: b1
   type(base(1)), allocatable :: b2
   type(base(1)), pointer :: b3

   call b1%assgn( 100_8 )
   print *, b1%i

   allocate ( b2, b3 )
   call b2%assgn( -128_4 )
   print *, b2%i

   call b3%assgn( 127_2 )
   print *, b3%i

   call b1%assgn(0_1)
   print *, b1%i

   call b2%assgn(0_2)
   print *, b2%i

   call b3%assgn(127)
   print *, b3%i

end program
