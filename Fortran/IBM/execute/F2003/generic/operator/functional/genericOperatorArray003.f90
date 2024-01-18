!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: Scalar to Array (**,//)
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
      integer(8) :: i = -999
      contains
         procedure, pass :: pow
         generic :: operator ( ** ) => pow
   end type

   type, extends(base) :: child
      character(3) :: c = 'xxx'
      contains
         generic :: operator ( // ) => concat
         procedure, pass :: concat
   end type

   interface operator( ** )
      class(base) function atoapow (a, b)
         import base
         class(base), intent(in) :: a(:), b(:)
         allocatable :: atoapow(:)
      end function
   end interface operator ( ** )

   interface operator( // )
      module procedure atoaconcat
   end interface operator ( // )

   contains

   function pow (a, b)
      class(base), intent(in) :: a, b(:)
      class(base), allocatable :: pow(:)

      allocate ( pow(size(b)), source = b )

      do i=1, size(b)
         pow(i)%i = a%i ** b(i)%i
      end do

   end function

   function concat (a, b)
      class(child), intent(in) :: a, b(:)
      class(child), allocatable :: concat(:)

      allocate ( concat(size(b)), source = b )

      do i=1, size(b)
         concat(i)%c = a%c(1:1) // b(i)%c(2:3)
      end do

   end function

   function atoaconcat (a, b)
      class(child), intent(in) :: a(:), b(:)
      class(child), allocatable :: atoaconcat(:)

      allocate ( atoaconcat(size(b)), source = b )
      if ( size ( a ) /= size ( b ) ) error stop 1_4

      do i=1, size(b)
         atoaconcat(i)%c = a(i)%c(1:1) // b(i)%c(2:3)
      end do

   end function

end module

function atoapow (a, b)
   use m, only: base
   class(base), intent(in) :: a(:), b(:)
   class(base), allocatable :: atoapow(:)

   allocate ( atoapow(size(b)) )

   do i=1, size(b)
      atoapow(i)%i = b(i)%i ** a(i)%i
   end do

end function

program genericOperatorScalar003
   use m

   class(base), allocatable :: b1, b2(:), b3(:), b4(:,:)
   class(child), pointer :: c1, c2(:), c3(:)
   type(child) c4(4)

   allocate ( b1, source = base (2) )
   allocate ( b2(4), source = b1 **  (/ ( base (i), i = 1, 4 ) /) )
   allocate ( b3(size(b2)), source = b1 ** b2 )

   print *, b1%i
   print *, b2%i
   print *, b3%i

   b2(4)%i = 4

   allocate ( b4(2,2), source = reshape ( source = ( b2 ** b2 ) , shape = (/2,2/) ) )
   print *, b4%i

   allocate ( c1, source = child ( 2, 'abc' ) )
   allocate ( c2(4), source = child ( 2, 'abc' ) // (/ child ( 2, 'ABC' ), child ( 2, 'DEF' ), child ( 2, 'GHI' ), child ( 2, 'JKL' ) /) )
   allocate ( c3(4), source = c2 // c2 )

   print *, c2%c
   print *, c3%c

   c4 = c2 // c3

   print *, c4%c

   deallocate ( b3 )
   allocate ( b3(4), source = c2 ** c3 )
   print *, b3%i


end program
