! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/mix/genericMix002.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : Mix generic type bounds
!*
!*  DESCRIPTION                : mix add, subtract, and assignment in a type and mix with intrinsic types
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

module myint

   type int(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains

         procedure, pass :: add_int_int
         procedure, pass :: add_int_integer
         procedure, pass(b) :: add_integer_int

         procedure, pass :: sub_int_int
         procedure, pass :: sub_int_integer
         procedure, pass(b) :: sub_integer_int

         procedure, pass    :: as_int_integer
         procedure, pass    :: as_int_int
         procedure, pass(b) :: as_integer_int

         generic :: operator(+) => add_int_int, add_int_integer, add_integer_int
         generic :: operator(-) => sub_int_int, sub_int_integer, sub_integer_int
         generic :: assignment(=) => as_int_integer, as_int_int, as_integer_int

   end type

   contains

      subroutine as_int_int(a,b)
         class(int(4)), intent(out) :: a
         class(int(4)), intent(in)  :: b

         print *, 'as_int_int'
         a%i = b%i

      end subroutine

      subroutine as_int_integer(a,b)
         class(int(4)), intent(out) :: a
         integer, intent(in)  :: b

         print *, 'as_int_integer'
         a%i = b

      end subroutine

      subroutine as_integer_int(a,b)
         class(int(4)), intent(in) :: b
         integer, intent(out)   :: a

         print *, 'as_integer_int'
         a = b%i

      end subroutine


      type(int(4)) function add_int_int ( a, b )
         class(int(4)), intent(in) :: a
         class(int(4)), intent(in) :: b

         print *, 'add_int_int'
         add_int_int%i = a%i + b%i

      end function

      type(int(4)) function add_int_integer ( a, b )
         class(int(4)), intent(in) :: a
         integer, intent(in) :: b

         print *, 'add_int_integer'
         add_int_integer%i = a%i + b

      end function

      type(int(4)) function add_integer_int ( a, b )
         integer, intent(in) :: a
         class(int(4)), intent(in) :: b

         print *, 'add_integer_int'
         add_integer_int%i = a + b%i

      end function

      type(int(4)) function sub_int_int ( a, b )
         class(int(4)), intent(in) :: a
         class(int(4)), intent(in) :: b

         print *, 'sub_int_int'
         sub_int_int%i = a%i - b%i

      end function

      type(int(4)) function sub_int_integer ( a, b )
         class(int(4)), intent(in) :: a
         integer, intent(in) :: b

         print *, 'sub_int_integer'
         sub_int_integer%i = a%i - b

      end function

      type(int(4)) function sub_integer_int ( a, b )
         integer, intent(in) :: a
         class(int(4)), intent(in) :: b

         print *, 'sub_integer_int'
         sub_integer_int%i = a - b%i

      end function

end module

program genericMix002
   use myint

   type(int(4)) :: i
   type(int(4)), allocatable :: j
   class(int(4)), pointer :: k

   integer :: ii
   integer, allocatable :: jj
   integer, pointer :: kk

   allocate ( k , j , jj , kk )

   print *, '*start for i:'
   i = int(4)(1) + 2 + int(4)(3) + 4 + 5
   print *, '*end:', i%i

   print *, '*start for ii:'
   ii = i
   print *, '*end:', ii

   print *, '*start for j:'
   j = i - 5 - int(4)(4) - 3 + 2 - int(4)(1)
   print *, '*end:', j%i

   print *, '*start for jj:'
   jj = 1 + 2 - 3 + 4
   print *, '*end:', jj

   print *, '*start for k:'
   k = 10 - 9 + 2
   print *, '*end:', k%i

   print *, '*start for kk:'
   kk = i + j - k + 10 - 20
   print *, '*end:', kk

end program