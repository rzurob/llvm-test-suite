!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: type bound procedure in base and generic defined in
!*                                         child type,
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
         procedure :: add
         procedure :: addwitharray
         generic :: operator(+) => add
   end type

   type, extends(base) :: child
      integer :: j
      contains
         generic :: operator(+) => addwitharray
   end type

   contains

   class(base) function add ( a, b )
      allocatable :: add
      class(base), intent(in) :: a, b

      allocate ( add, source= base ( i = a%i + b%i ) )
   end function

   class(base) function addwitharray ( a, b )
      allocatable :: addwitharray(:)
      class(base), intent(in) :: a, b(:)

      allocate ( addwitharray(size(b)) )
      do i=1, size(b)
         addwitharray(i)%i =  a%i + b(i)%i
      end do
   end function

end module

program genericOperatorScalar016d
   use m

   type(base) :: b1, b2(3)

   b1 = base(1)
   b2 = base(2)

   b2 = b1 + b2

end program
