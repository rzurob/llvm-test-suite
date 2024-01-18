!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Functional test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The use of procedure pointers with the
!*                               interfaces declared in an abstract interface
!*                               They can point to any procedures as long as
!*                               they have the same interface
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

abstract interface
   function func (x)
      integer, intent (in) :: x
      logical :: func
   end function

   subroutine print_out(n, result)
      integer, intent (in) :: n
      logical, intent (in) :: result
   end subroutine
end interface

logical result
integer n

!-- a derived type with a procedure pointer component
type struct_type
   procedure (func), pointer, nopass :: component =>null()
end type struct_type
! Procedures with the same interface as defined in an abstract interface
procedure (func) prime
procedure (print_out) print_result
! procedure pointers with explicit interface and initialized to null().
procedure (func), pointer :: p => null()
procedure (print_out), pointer :: r => null()

!-- ... and a variable of that type.
type(struct_type) :: struct

! prime and print_result are procedure pointers with a
! compatible interface.
p => prime
r => print_result
n = 10
result = p(10)
call r(n, result)

! likewise for a structure component.
struct % component => prime
n = 11
result = struct % component(n)
call r(n, result)
end

logical function prime(n)
   integer i, j
   i = 2
   j = sqrt(real(n))
   do  10, while ((i .le. j) .and. (mod(n, i) .ne. 0))
      i = i + 1
10 continue
   if( i .gt. j) then
      prime = .true.
   else
      prime = .false.
   end if
end  function

subroutine print_result(n, result)
   integer, intent (in) :: n
   logical, intent (in) :: result
   if ( result ) then
      print *, n, ' is a prime number'
   else
      print *, n, ' is not a prime number'
   end if
end subroutine
