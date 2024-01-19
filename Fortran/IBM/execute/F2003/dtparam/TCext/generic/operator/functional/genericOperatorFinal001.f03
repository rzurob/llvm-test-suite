! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorFinal001.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: finalization inside used-defined operator
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
      integer, kind            :: k1
      integer, len             :: n1
      integer(k1), allocatable :: i
      contains
         procedure :: neg
         generic :: operator(-) => neg
         final :: finalscalarbase
   end type

   contains

      type(base(20,4)) function neg ( a )
         class(base(*,4)), intent(in) :: a

         print *, 'inside neg'

         if ( .not. allocated (neg%i ) ) allocate ( neg%i )
         neg%i = a%i * (-1)

      end function

      subroutine finalscalarbase ( a )
         type(base(*,4)), intent(inout) :: a

         if (allocated(a%i)) then
            print *, 'finalized :', a%i
        else
            print *, 'finalized :', 0
        end if

         if ( allocated( a%i ) )         deallocate ( a%i )

      end subroutine

end module

program genericOperatorFinal001
   use m

   type(base(20,4)) :: b1
   type(base(:,4)), allocatable :: b2

   b1 = base(20,4) ( 10 )
   allocate (b2, source = base(20,4) (20) )

   b1 = -b2
   b2 = -b1

end program
