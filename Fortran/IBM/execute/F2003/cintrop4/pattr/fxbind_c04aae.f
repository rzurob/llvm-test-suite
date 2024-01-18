! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran programs interoperate with C functions
!*                                through a Fortran procedure interface that uses
!*                                the BIND specification .
!*                              - interop functions contained in Module
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with derived type.
!*   - The interoperable  procedure itself is  implemented as C function.
!*   - The interoperabl Fortran procedure  has an explicit interface and
!*     is declared with the BIND attribute.
!*   - passing scalar arguments by REFERENCE and by VALUE
!*   - main written in FORTRAN, Fortran calls C functions.
!*
!*  ALGORITHM :
!*          1. Declare the interop functions in Fortran program.
!*          ( Create a procedural interface that corresponds to the C prototype
!*          and bind the interface to the C function using the BIND(C) specifier).
!*          2. Initialize the variable which will be the  actual arguments of
!*             the interop functions.
!*          3. Fortran  program call C function.The argument is  altered
!*             during execution of the C Function.
!*          4. Assertion: Check the modified auguments and return value
!*             in Fortran to verify it is correct.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module deraae
  use iso_c_binding
  type,bind(c):: der_bind
     real*16 r16
     real*8 r8
     real*4 r4

     integer*8 i8
     integer*4 i4
     integer*2 i2
     integer*1 i1

     logical*1 l1
     character*1 c

  end type der_bind

end module deraae

module mint
  use iso_c_binding
  use deraae
  interface

     function fun_der_ref(der_bind_ref) BIND(C)
       use iso_c_binding
       use deraae
       type(der_bind), intent(inout) :: der_bind_ref
       integer fun_der_ref
     end function fun_der_ref

     function fun_der_val(der_bind_val) BIND(C)
       use iso_c_binding
       use deraae
       type(der_bind), value :: der_bind_val
       integer fun_der_val
     end function fun_der_val

  end interface

end module mint

program fxbind_c04aae
  use iso_c_binding
  use deraae
  use  mint
  type(der_bind) ::  der_ref, der_val,der_ref_compare,der_val_compare

  integer tmp

  !**********************************************************
  !        Initialization of variables                      *
  !**********************************************************

  der_ref%i1 = 5
  der_ref_compare%i1 = 8
  der_ref%i2 = 15
  der_ref_compare%i2 = 18
  der_ref%i4 = 11
  der_ref_compare%i4 = 14
  der_ref%i8 = 17
  der_ref_compare%i8 = 20

  der_ref%r4 = 4.80
  der_ref_compare%r4 = 9.6
  der_ref%r8 = 140.8
  der_ref_compare%r8 = 281.6
  der_ref%r16 = 1600.3
  der_ref_compare%r16 = 3200.6
  der_ref%l1 = .false.
  der_ref_compare%l1 = .true.
  der_ref%c = 'a'
  der_ref_compare%c = 'd'

  der_val%i1 = 5
  der_val_compare%i1 = 5
  der_val%i2 = 15
  der_val_compare%i2 = 15
  der_val%i4 = 11
  der_val_compare%i4 = 11
  der_val%i8 = 17
  der_val_compare%i8 = 17
  der_val%r4 = 4.80
  der_val_compare%r4 = 4.80
  der_val%r8 = 140.8
  der_val_compare%r8 = 140.8
  der_val%r16 = 1600.3
  der_val_compare%r16 = 1600.3
  der_val%l1 = .false.
  der_val_compare%l1 = .false.
  der_val%c = 'a'
  der_val_compare%c = 'a'

  !**********************************************************
  ! Calling C from Fortran with derive type with different
  !          data type and check the results
  !**********************************************************

  ! Test 1 : call by reference
  ! A dummy argument without the VALUE attribute correspond
  ! to a formal parameter  of the prototype in C program
  ! that is of a pointer type.

  tmp = fun_der_ref(der_ref)
  if(der_ref%i1 .ne. der_ref_compare%i1) error stop 10
  if(der_ref%i2 .ne. der_ref_compare%i2) error stop 11
  if(der_ref%i4 .ne. der_ref_compare%i4) error stop 12
  if(der_ref%i8 .ne. der_ref_compare%i8) error stop 13

  if(der_ref%r4 .ne. der_ref_compare%r4) error stop 14
  if(der_ref%r8 .ne. der_ref_compare%r8) error stop 15
  if(der_ref%r16 .ne. der_ref_compare%r16) error stop 16

  if(der_ref%c .ne. der_ref_compare%c) error stop 17

  ! Test 2 : call by value
  ! A dummy argument with the VALUE attribute  correspond
  ! to a formal parameter of the prototype in C program that is
  ! not of a pointer type.

  tmp = fun_der_val(der_val)
  if(der_val%i1 .ne. der_val_compare%i1) error stop 30
  if(der_val%i2 .ne. der_val_compare%i2) error stop 31
  if(der_val%i4 .ne. der_val_compare%i4) error stop 32
  if(der_val%i8 .ne. der_val_compare%i8) error stop 33

  if(der_val%r4 .ne. der_val_compare%r4) error stop 34
  if(der_val%r8 .ne. der_val_compare%r8) error stop 35
  if(der_val%r16 .ne. der_val_compare%r16) error stop 36

  if(der_val%c .ne. der_val_compare%c) error stop 37

end program fxbind_c04aae
