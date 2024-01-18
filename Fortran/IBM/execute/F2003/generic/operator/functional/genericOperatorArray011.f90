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
!*  DESCRIPTION                : Operator: with array and class hierarchy
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
         procedure :: addwitharray
         procedure :: addwithintarray   !<- generic only be used in type child
         generic :: operator(+) => addwitharray
   end type

   type, extends(base) :: child
      contains
         generic :: operator(+) => addwithintarray
   end type

   contains

   class(base) function addwithintarray ( a, b )
      allocatable :: addwithintarray(:)
      class(base), intent(in) :: a
      integer, intent(in) :: b(:)

      allocate ( addwithintarray(size(b)) )

      do i=1, size(b)
         addwithintarray(i)%i = a%i + b(i)
      end do

   end function

   class(base) function addwitharray ( a, b )
      allocatable :: addwitharray(:)
      class(base), intent(in) :: a, b(:)

      allocate ( child :: addwitharray(size(b)) )

      do i=1, size(b)
         addwitharray(i)%i  =  a%i + b(i)%i
      end do

   end function

end module

program genericOperatorArray011
   use m

   class(base), allocatable :: b1(:), b3(:)
   type(base) :: b2(3)

   allocate ( b1(3), source = (/ (child(i), i = 11, 13) /) )

   b2 =  b1(1)+ b1
   print *, b2%i

   select type ( g => b1(1) )
      type is (child)
         allocate ( b3(3), source = g + (/ -10, -11, -12 /) )
         print *, b3%i
   end select

   deallocate (b3)

   allocate ( b3(3), source = addwithintarray( b1(1), (/ -20, -21, -22 /) ) )
   print *, b3%i

end program
