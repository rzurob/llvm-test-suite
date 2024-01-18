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
!*  SECONDARY FUNCTIONS TESTED : Mix generic type bounds
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : mix add, subtract, and assignment in a type with large matrix operations
!*
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

   type int
      integer :: i
      contains

         procedure, pass, private :: add_int_int
         procedure, pass, private :: sub_int_int
         procedure, pass, private :: mul_int_int
         procedure, pass, private :: as_int_int

         generic :: operator(+) => add_int_int
         generic :: operator(-) => sub_int_int
         generic :: operator(*) => mul_int_int
         generic :: assignment(=) => as_int_int

   end type

   contains

      elemental subroutine as_int_int(a,b)
         class(int), intent(out) :: a
         class(int), intent(in)  :: b

         a%i = b%i

      end subroutine

      elemental type(int) function add_int_int ( a, b )
         class(int), intent(in) :: a
         class(int), intent(in) :: b

         add_int_int%i = a%i + b%i

      end function

      elemental type(int) function mul_int_int ( a, b )
         class(int), intent(in) :: a
         class(int), intent(in) :: b

         mul_int_int%i = a%i * b%i

      end function

      elemental type(int) function sub_int_int ( a, b )
         class(int), intent(in) :: a
         class(int), intent(in) :: b

         sub_int_int%i = a%i - b%i

      end function

end module

program genericMix004
   use myint

   type(int), allocatable :: a(:,:)
   type(int) :: b(512,512)
   type(int), pointer :: c(:,:)

   allocate ( a(512,512), c(512,512) )

   ! initialize values

   do i = 1, 512
      do j = 1, 512
         a(i,j) = int(i+j)
      end do
   end do

   do i = 1, 512
      do j = 1, 512
         b(i,j) = int(-1) * int(i+j+k)
      end do
   end do

   ! Computations

   do i = 1, 512
      do j = 1, 512
         c(i,j) = a(i,j) + b(i,j) - a(i,j) - b(i,j)
      end do
   end do

   do i = 1, 512
      do j = 1, 512
         if ( c(i,j)%i .ne. 0 ) then
            print *, 'Coordinates:', i, j, c(i,j)%i
            error stop 1_4
         end if
      end do
   end do

   ! initialize i to be identity matrix

   do i = 1, 512
      do j = 1, 512
         if ( i .eq. j ) then
            a(i,j) = int(1)
         else
            a(i,j) = int(0)
         end if
         c(i,j) = int(0)
      end do
   end do

   ! Matrix Multiplication

   do i = 1, 512
      do j = 1, 512
         do k = 1, 512
            c(i,j) = c(i,j) + a(i,k) * b(k,j)
         end do
      end do
   end do

   ! c should be equal to b

   do i = 1, 512
      do j = 1, 512
         if ( c(i,j)%i .ne. b(i,j)%i ) then
            print *, 'Coordinates:', i, j, c(i,j)%i, b(i,j)%i, a(i,j)%i
            error stop 2_4
         end if
      end do
   end do

end program
