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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
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

   type base
      integer, allocatable :: i
      contains
         procedure :: neg
         generic :: operator(-) => neg
         final :: finalscalarbase
   end type

   contains

      type(base) function neg ( a )
         class(base), intent(in) :: a

         print *, 'inside neg'
         
         if ( .not. allocated (neg%i ) ) allocate ( neg%i )
         neg%i = a%i * (-1)

      end function

      subroutine finalscalarbase ( a )
         type(base), intent(inout) :: a

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

   type(base) :: b1
   type(base), allocatable :: b2

   b1 = base ( 10 )
   allocate (b2, source = base (20) )
   
   b1 = -b2	
   b2 = -b1

end program
