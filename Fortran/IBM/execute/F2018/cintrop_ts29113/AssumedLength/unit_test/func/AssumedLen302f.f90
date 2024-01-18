! *********************************************************************
!* ===================================================================
!*
!* DATE                         : June  25, 2014
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed length object
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    : -qdebug = BCASSUMEDLEN  (temporarily)
!*
!* DESCRIPTION                  : Calling a C BIND(C) procedure from Fortran
!*
!*                                - type character(*)
!*                                - Dummy argument is an assumed_size array,
!*                                    whose shape depends on another argument.
!*                                - Call to BIND(C) procedure from main,
!*                                      internal/external procedure
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

Program main
 interface
   subroutine sub_1(arg1,n) bind(c)
     integer n
     character(*) :: arg1(n,*)
   end subroutine
   subroutine sub_3(arg) bind(c)
     character(:), allocatable :: arg(:,:)
   end subroutine
 end interface


 ! explicit_shape  array
 character(3) :: c1(8)
 character(3) :: c11(4,2)

 ! deffered_shape array with allocatable attribute
 character(:), allocatable :: c2(:)
 character(3), allocatable :: c22(:)

 ! defered_shape array with pointer attribute which will be allocated
 character(:), pointer :: c3(:)
 character(3), pointer :: c33(:)

 ! deffered_shape array with pointer attribute which will be associated
 character(:), pointer :: c4(:)
 character(3), target :: t4(8)

 ! implied_shape array
 character(3), parameter :: c5(*, 3:*) = RESHAPE(["AAA","BBB","CCC","DDD","EEE","FFF","GGG","HHH"], [4, 2])

 ! defered_shape array
 character(:), allocatable :: c6(:,:)

 ! constant expression array
 character(3), parameter :: c7(4,2) = RESHAPE(["AAA","BBB","CCC","DDD","EEE","FFF","GGG","HHH"], [4,2])

 integer :: k

 ! c1
 do i=1,8
   c1(i) = CHAR(i+64) // CHAR(i+64) // CHAR(i+64)
 end do

 !c11
 k = 0
 do i=1,2
   do j=1,4
     k = k + 1
     c11(j,i) = CHAR(k+64) // CHAR(k+64) // CHAR(k+64)
   end do
 end do

 ! c2
 allocate(character(3) :: c2(8))
 do i = 1,8
   c2(i) = CHAR(i+64) // CHAR(i+64) // CHAR(i+64)
 end do

 ! c22
 allocate(c22(8))
 do i = 1,8
   c22(i) = CHAR(i+64) // CHAR(i+64) // CHAR(i+64)
 end do

 ! c3
 allocate(character(3) :: c3(8))
 do i = 1,8
   c3(i) = CHAR(i+64) // CHAR(i+64) // CHAR(i+64)
 end do

 ! c33
 allocate(c33(8))
 do i = 1,8
   c33(i) = CHAR(i+64) // CHAR(i+64) // CHAR(i+64)
 end do

 ! c4
 do i = 1,8
   t4(i) = CHAR(i+64) // CHAR(i+64) // CHAR(i+64)
 end do

 c4 => t4

 allocate(character(3) :: c6(4,2))
 c6 =  RESHAPE(["AAA","BBB","CCC","DDD","EEE","FFF","GGG","HHH"], [4, 2])

 !************ calling sub_1 routine with different possible actual arguments **************!

 !!---- actual argument for sub_1 is explicit_shape array
 call sub_1(c1,2)
 call sub_1(c11,2)

 !!---- actual argument for sub_1 is defered_shape array
 call sub_1(c2,2)
 call sub_1(c22,2)
 call sub_1(c3,2)
 call sub_1(c33,2)
 call sub_1(c4,2)

 !!---- actual argument for sub_1 is implied_shape array
 call sub_1(c5,2)

 !!---- actual argument for sub_1 is an expression array
 call sub_1(c1 // "",2)

 call sub_int1(c1)

 call sub_int2(c11)

call sub_3(c6)

contains
  subroutine sub_int1(arg2)
    character(3) :: arg2(2,*)
 !!---- actual argument for sub_1 is an assumed_size array
    call sub_1(arg2,2)
  end subroutine
  subroutine sub_int2(arg3)
    character(3):: arg3(1:,:)
 !!---- actual argument for sub_1 is an assumed_shape array
    call sub_1(arg3,2)
  end subroutine

end program

! Definition of sub_3 subroutine :
subroutine sub_3(arg) bind(c)
  character(:), allocatable :: arg(:,:)
  interface
   subroutine sub_1(arg1,n) bind(c)
    integer n
    character(*) :: arg1(n,*)
   end subroutine
  end interface
 !!---- actual argument for sub_1 is a CFI descriptor
  call sub_1(arg, 2)
end subroutine



