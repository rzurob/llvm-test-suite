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

   type base
      integer, allocatable :: i
      contains
         procedure :: add
         procedure :: addarray
         generic :: operator(+) => add, addarray
         final :: finalscalarbase, final1darraybase
   end type

   contains

      type(base) function add ( a, b )
         class(base), intent(in) :: a, b

         print *, 'inside add'

         if ( .not. allocated (add%i ) ) allocate ( add%i )
         add%i = a%i + b%i

      end function

      type(base) function addarray ( a, b )
         class(base), intent(in) :: a, b(:)
         allocatable :: addarray(:)

         print *, 'inside addarray'

         allocate ( addarray(size(b)), source = b )

         do j=1,size(addarray)
            if ( .not. allocated (addarray(j)%i ) ) allocate ( addarray(j)%i )
            addarray(j)%i = a%i * addarray(j)%i
         end do

      end function

      subroutine finalscalarbase ( a )
         type(base), intent(inout) :: a

         print *, 'finalized :'

         if ( allocated( a%i ) )  then
             print *, a%i
             deallocate ( a%i )
         end if

      end subroutine

      subroutine final1darraybase ( a )
         type(base), intent(inout) :: a(:)

         print *, 'finalized array:'

         do j=1, size(a)
            if ( allocated( a(j)%i ) )  then
                print *, a(j)%i
                deallocate ( a(j)%i )
            end if
         end do

      end subroutine

end module

program genericOperatorFinal002
   use m

   type(base) :: b1, b3(4)
   type(base), allocatable :: b2, b4(:)

   b1 = base ( 10 )
   allocate (b2, source = base (20) )

   b3 = (/ base(1), base(2), base(3), base(4) /)

   allocate ( b4(5), source = (/ b3, base(5) /) )

   print *, 'end of init'

   b1 = b1 + b2

   b3 = b1 + b4

   b4 = b2 + b3

end program
