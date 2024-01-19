! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentEntry001.f
! opt variations: -qnol -qdeferredlp -qreuse=none

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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, pass :: bassgnint
         procedure, pass :: bassgnreal
         generic :: assignment(=) => bassgnint, bassgnreal
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
      contains
         procedure, pass :: bassgnint => cassgnint
         procedure, pass :: bassgnreal => cassgnreal
   end type

   contains

      subroutine bassgnint ( a, b )
         class(base(*,4)), intent(out) :: a
         integer(4), intent(in) :: b
         real(4), intent(in) :: c

         a%i = b
         goto 100

         entry bassgnreal ( a, c )

            a%i = int( c, kind=4)

  100       print *, a%i, 'is assigned'

      end subroutine

      subroutine cassgnint ( a, b )
         class(child(*,4)), intent(out) :: a
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

   class(base(20,4)), allocatable :: b1
   class(base(20,4)), pointer     :: b2

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
