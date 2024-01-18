!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: operands with scalar and entry statement
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

   type base
      integer :: i
      contains
         procedure, pass :: bassgnint
         procedure, pass :: bassgnreal
         generic :: assignment(=) => bassgnint, bassgnreal
   end type

   type, extends(base) :: child
      integer :: j
      contains
         procedure, pass :: bassgnint => cassgnint
         procedure, pass :: bassgnreal => cassgnreal
   end type

   contains

      subroutine bassgnint ( a, b )
         class(base), intent(out) :: a
         integer(4), intent(in) :: b
         real(4), intent(in) :: c

         a%i = b
         goto 100

         entry bassgnreal ( a, c )

            a%i = int( c, kind=4)

  100       print *, a%i, 'is assigned'

      end subroutine

      subroutine cassgnint ( a, b )
         class(child), intent(out) :: a
         integer(4), intent(in) :: b
         real(4), intent(in) :: c

         a%i = b
         a%j = b
         goto 100

         entry cassgnreal ( a, c )

            a%i = int( c, kind=4)
            a%j = int( c, kind=4)

  100       print *, a%i, 'is assigned'
            print *, a%j, 'is assigned'

      end subroutine

end module

program genericAssignmentEntry001
   use m

   class(base), allocatable :: b1
   class(base), pointer     :: b2

   allocate ( b1, b2 )

   b1 = 10
   print *, b1%i
   b1 = 20.00_4
   print *, b1%i

   b2 = 10 + 100 + 200 - 110
   print *, b2%i
   b2 = 100.0 * 2.0 / 0.5
   print *, b2%i

end program
