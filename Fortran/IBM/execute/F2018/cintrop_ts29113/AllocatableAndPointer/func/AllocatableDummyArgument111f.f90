! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument106f.f
!*
!* PROGRAMMER                   : Dorra Bouchiha
!* DATE                         : January 25, 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a Fortran BIND(C) procedure from Fortran
!*
!*                                - Allocatable scalar/array of various interoperable types
!*                                - Array constructor 
!*                                - move_alloc
!*                                - Generic resolution based
!*                                  * on type
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program AllocatableDummyArgument111f
  use, intrinsic :: ieee_arithmetic
  implicit none

  interface my_allocate
     subroutine my_allocate_int2(a, b, n) bind(c)
        use iso_c_binding
        implicit none
        integer :: n
        integer(c_short) :: b(n)
        integer(c_short), allocatable  :: a(:)
     end subroutine my_allocate_int2
     subroutine my_allocate_float(a, b, n) bind(c)
        use iso_c_binding
        implicit none
        integer :: n
        real(c_float) :: b(n)
        real(c_float), allocatable  :: a(:)
     end subroutine my_allocate_float
     subroutine my_allocate_double(a, b, n) bind(c)
        use iso_c_binding
        implicit none
        integer :: n
        real(c_double) :: b(n)
        real(c_double), allocatable :: a(:)
     end subroutine my_allocate_double
     subroutine my_allocate_bool(a, b, n) bind(c)
        use iso_c_binding
        implicit none
        integer :: n
        logical(c_bool) :: b(n)
        logical(c_bool), allocatable :: a(:)
     end subroutine my_allocate_bool
     subroutine my_allocate_char(a, b, n) bind(c)
        use iso_c_binding
        implicit none
        integer :: n
        character(c_char) :: b(n)
        character(:), allocatable :: a(:)
     end subroutine my_allocate_char
     subroutine my_allocate_cmplx(a, b, n) bind(c)
        use iso_c_binding
        implicit none
        integer :: n
        complex(c_float_complex) :: b(n)
        complex(c_float_complex), allocatable :: a(:)
     end subroutine my_allocate_cmplx
     subroutine my_allocate_dcmplx(a, b, n) bind(c)
        use iso_c_binding
        implicit none
        integer :: n
        complex(c_double_complex) :: b(n)
        complex(c_double_complex), allocatable :: a(:)
     end subroutine my_allocate_dcmplx
  end interface

  integer (2), allocatable      :: iarr(:)
  real    (4), allocatable      :: rarr(:), rarx(:)
  logical (1), allocatable      :: larr(:)
  character(:), allocatable     :: carr(:)
  complex (4), allocatable      :: zarr(:), zarx(:)
  double complex, allocatable   :: dzarr(:), dzarx(:)
  double precision, allocatable :: dparr(:), dparx(:)

  integer :: i

! Type integer(2)
  if (allocated(iarr)) error stop 10
  call my_allocate(iarr, [integer(2):: 32767, 0, 2000000000], 3)
  if (allocated(iarr)) error stop 11
  call my_allocate(iarr, [integer(2):: (32767, 0, 2000000000, i=1,1)], 3)
  if (allocated(iarr)) error stop 12

! Type real(4)
  rarx = [real(4):: 0.0]
  if (allocated(rarr)) error stop 13
  call my_allocate(a=rarr, n=5, b=[real(4):: 3.2767, -rarx(1), 9.87654321e32, &
                                       ieee_value(rarx(1),IEEE_POSITIVE_INF), &
                                           ieee_value(rarx(1),IEEE_QUIET_NAN)])
  if (allocated(rarr)) error stop 13
  call my_allocate(rarr, [real(4):: (3.2767, -0.0, 9.87654321e32, &
                            ieee_value(rarx(1),IEEE_POSITIVE_INF), &
                     ieee_value(rarx(1),IEEE_QUIET_NAN), i=1,1)], 5)
  if (allocated(rarr)) error stop 13

! Type logical(1)
  if (allocated(larr)) error stop 13
  call my_allocate(larr, [logical(1):: .true., .false.], 2)
  if (allocated(larr)) error stop 13
  call my_allocate(larr, [logical(1):: (.true., .false., i=1,1)], 2)
  if (allocated(larr)) error stop 13

! Type character(1)
  if (allocated(carr)) error stop 13
  call my_allocate(carr, [character(1):: 'a', 'b', 'c'], 3)
  if (allocated(carr)) error stop 13
  call my_allocate(carr, [character(1):: ('a', 'b', 'c', i=1,1)], 3)
  if (allocated(carr)) error stop 13

! Type complex(4)
  zarx = [complex(4):: (0.0,0.0)]
  if (allocated(zarr)) error stop 13
  call my_allocate(zarr, [complex(4):: (3.2767,-0.0), &
                  (9.87654321e32, ieee_value(real(zarx(1)),IEEE_POSITIVE_INF)), &
                              (ieee_value(aimag(zarx(1)),IEEE_QUIET_NAN), ieee_value(aimag(zarx(1)),IEEE_POSITIVE_NORMAL))], 3)
  if (allocated(zarr)) error stop 13
  call my_allocate(zarr, [complex(4):: ((3.2767,-0.0), &
                  (9.87654321e32, ieee_value(real(zarx(1)),IEEE_POSITIVE_INF)), &
                  (ieee_value(aimag(zarx(1)),IEEE_QUIET_NAN), ieee_value(aimag(zarx(1)),IEEE_POSITIVE_NORMAL)), i=1,1)], 3)
  if (allocated(zarr)) error stop 13

! Type double complex
  dzarx = [double complex:: (0.0d0,0.0d0)]
  if (allocated(dzarr)) error stop 13
  call my_allocate(dzarr, [double complex:: (3.2767d300,-0.0d200), &
                  (-9.87654321d32, ieee_value(real(dzarx(1)),IEEE_NEGATIVE_INF)), &
                  (ieee_value(aimag(dzarx(1)),IEEE_QUIET_NAN), ieee_value(aimag(dzarx(1)),IEEE_NEGATIVE_NORMAL))], 3)
  if (allocated(dzarr)) error stop 13
  call my_allocate(a=dzarr, n=3, b=[double complex:: ((3.2767d300,-0.0d200), &
                  (-9.87654321d32, ieee_value(real(dzarx(1)),IEEE_NEGATIVE_INF)), &
                  (ieee_value(aimag(dzarx(1)),IEEE_QUIET_NAN), ieee_value(aimag(dzarx(1)),IEEE_NEGATIVE_NORMAL)), i=1,1)])
  if (allocated(dzarr)) error stop 13

! Type double precision
  dparx = [double precision:: 0.0d0]
  if (allocated(dparr)) error stop 13
  call my_allocate(dparr, [double precision:: ieee_value(real(dparx(1)),IEEE_NEGATIVE_INF), 0.0d300, 1d-9], 3)
  if (allocated(dparr)) error stop 13
  call my_allocate(dparr, [double precision:: (ieee_value(real(dparx(1)),IEEE_NEGATIVE_INF), 0.0d300, 1d-9, i=1,1)], 3)
  if (allocated(dparr)) error stop 13

  ! Try a couple of empty arrays:
  if (allocated(iarr)) error stop 13
  call my_allocate(iarr, [integer(2):: ], 0)
  if (allocated(iarr)) error stop 13

  call my_allocate(iarr, [integer(2):: (i, i=1,0)], 0)
  if (allocated(iarr)) error stop 13

  if (allocated(zarr)) error stop 13
  call my_allocate (zarr, [complex(4):: ((real(i),real(i)), i=1,0)], 0)
  if (allocated(zarr)) error stop 13

end program AllocatableDummyArgument111f

subroutine my_allocate_int2(a, b, n) bind(c)
   use iso_c_binding
   implicit none
   integer :: n
   integer(c_short) :: b(n)
   integer(c_short), allocatable  :: a(:)

   allocate (a(n), source=b)
   print*, a

   if (.not. allocated(a)) error stop 10
   call my_move_alloc(a) 
   if (      allocated(a)) error stop 11

   contains 
   subroutine my_move_alloc(from)
     integer(2), allocatable :: from(:), to(:)

     if (.not. allocated(from)) error stop 10
     if (        allocated(to)) error stop 11
     call move_alloc(from, to)
     if (      allocated(from)) error stop 12
     if ( .not.  allocated(to)) error stop 13
   end subroutine my_move_alloc
end subroutine my_allocate_int2

subroutine my_allocate_float(a, b, n) bind(c)
   use iso_c_binding
   implicit none
   integer :: n
   real(c_float) :: b(n)
   real(c_float), allocatable  :: a(:)

   allocate (a(n), source=b)
   print*, a

   if (.not. allocated(a)) error stop 10
   call my_move_alloc(a) 
   if (      allocated(a)) error stop 11

   contains 
   subroutine my_move_alloc(from)
     real(4), allocatable :: from(:), to(:)

     if (.not. allocated(from)) error stop 10
     if (        allocated(to)) error stop 11
     call move_alloc(from, to)
     if (      allocated(from)) error stop 12
     if ( .not.  allocated(to)) error stop 13
   end subroutine my_move_alloc
end subroutine my_allocate_float

subroutine my_allocate_double(a, b, n) bind(c)
   use iso_c_binding
   implicit none
   integer :: n
   real(c_double) :: b(n)
   real(c_double), allocatable :: a(:)

   allocate (a(n), source=b)
   print*, a

   if (.not. allocated(a)) error stop 10
   call my_move_alloc(a) 
   if (      allocated(a)) error stop 11

   contains 
   subroutine my_move_alloc(from)
     double precision, allocatable :: from(:), to(:)

     if (.not. allocated(from)) error stop 10
     if (        allocated(to)) error stop 11
     call move_alloc(from, to)
     if (      allocated(from)) error stop 12
     if ( .not.  allocated(to)) error stop 13
   end subroutine my_move_alloc
end subroutine my_allocate_double

subroutine my_allocate_bool(a, b, n) bind(c)
   use iso_c_binding
   implicit none
   integer :: n
   logical(c_bool) :: b(n)
   logical(c_bool), allocatable :: a(:)

   allocate (a(n), source=b)
   print*, a

   if (.not. allocated(a)) error stop 10
   call my_move_alloc(a) 
   if (      allocated(a)) error stop 11

   contains 
   subroutine my_move_alloc(from)
     logical(1), allocatable :: from(:), to(:)

     if (.not. allocated(from)) error stop 10
     if (        allocated(to)) error stop 11
     call move_alloc(from, to)
     if (      allocated(from)) error stop 12
     if ( .not.  allocated(to)) error stop 13
   end subroutine my_move_alloc
end subroutine my_allocate_bool

subroutine my_allocate_char(a, b, n) bind(c)
   use iso_c_binding
   implicit none
   integer :: n
   character(c_char) :: b(n)
   character(:), allocatable :: a(:)

   allocate (a(n), source=b)
   print*, a

   if (.not. allocated(a)) error stop 10
   call my_move_alloc(a) 
   if (      allocated(a)) error stop 11

   contains 
   subroutine my_move_alloc(from)
     character(:), allocatable :: from(:), to(:)

     if (.not. allocated(from)) error stop 10
     if (        allocated(to)) error stop 11
     call move_alloc(from, to)
     if (      allocated(from)) error stop 12
     if ( .not.  allocated(to)) error stop 13
   end subroutine my_move_alloc
end subroutine my_allocate_char

subroutine my_allocate_cmplx(a, b, n) bind(c)
   use iso_c_binding
   implicit none
   integer :: n
   complex(c_float_complex) :: b(n)
   complex(c_float_complex), allocatable :: a(:)

   allocate (a(n), source=b)
   print*, a

   if (.not. allocated(a)) error stop 10
   call my_move_alloc(a) 
   if (      allocated(a)) error stop 11

   contains 
   subroutine my_move_alloc(from)
     complex(4), allocatable :: from(:), to(:)

     if (.not. allocated(from)) error stop 10
     if (        allocated(to)) error stop 11
     call move_alloc(from, to)
     if (      allocated(from)) error stop 12
     if ( .not.  allocated(to)) error stop 13
   end subroutine my_move_alloc
end subroutine my_allocate_cmplx

subroutine my_allocate_dcmplx(a, b, n) bind(c)
   use iso_c_binding
   implicit none
   integer :: n
   complex(c_double_complex) :: b(n)
   complex(c_double_complex), allocatable :: a(:)

   allocate (a(n), source=b)
   print*, a

   if (.not. allocated(a)) error stop 10
   call my_move_alloc(a) 
   if (      allocated(a)) error stop 11

   contains 
   subroutine my_move_alloc(from)
     double complex, allocatable :: from(:), to(:)

     if (.not. allocated(from)) error stop 10
     if (        allocated(to)) error stop 11
     call move_alloc(from, to)
     if (      allocated(from)) error stop 12
     if ( .not.  allocated(to)) error stop 13
   end subroutine my_move_alloc
end subroutine my_allocate_dcmplx
