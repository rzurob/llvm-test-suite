! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AssumedLen201f.f
!*
!* PROGRAMMER                   : Maryam Moghadas
!* DATE                         : June  25, 2014
!* ORIGIN                       : AIX Complier Development
!*
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed length object
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    : -qdebug = BCASSUMEDLEN  (temporarily)
!*
!* DESCRIPTION                  : Calling a C BIND(C) procedure from Fortran
!*
!*                                - type character(*)
!*                                - Dummy argument is an explicit_shape array(rank=3)
!*                                    all possible actual arguments are tested
!*                                - Call to BIND(C) procedure from different scopes:
!*                                      main program, internal/external procedure
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

Program AssumedLen 
! dummy argument is explicit shape 
 interface
   subroutine sub_1(arg1) bind(c)
     character(*) :: arg1(2,2,3)
   end subroutine
   subroutine sub_3(arg) bind(c)
     character(:), allocatable :: arg(:,:)
 !6- actual argument for sub_1 is a CFI descriptor
   end subroutine
 end interface 

 
 ! explicit_shape  array 
 character(1) :: c1(25) 
 character(1) :: c11(2,2,3)
 
 ! deffered_shape array with allocatable attribute 
 character(:), allocatable :: c2(:) 
 character(1), allocatable :: c22(:)

 ! defered_shape array with pointer attribute which will be allocated 
 character(:), pointer :: c3(:)
 character(1), pointer :: c33(:)
  
 ! deffered_shape array with pointer attribute which will be associated 
 character(:), pointer :: c4(:)
 character(1), target :: t4(30)

 ! implied_shape array
 character(1), parameter :: c5(*, 3:*, *) = RESHAPE(["A","B","C","D","E","F","G","H","I","J","K","L"], [2,2,3])

 ! defered_shape array 
 character(:), allocatable :: c6(:,:)

 character(1), parameter :: c7(2,2,3) = RESHAPE(["A","B","C","D","E","F","G","H","I","J","K","L"], [2,2,3]) 

 integer :: k 

 ! c1 
 do i=1, 25
   c1(i) = CHAR(i+64)  
 end do

 !c11
 k = 0
 do i=1,3
   do j=1,2
     do t=1,2
       k = k + 1
       c11(t,j,i) = CHAR(k+64) 
     end do   
   end do
 end do

 ! c2 
 allocate(character(1) :: c2(12))
 do i = 1,12
   c2(i) = CHAR(i+64) 
 end do

 ! c22
 allocate(c22(12))
 do i = 1,12
   c22(i) = CHAR(i+64) 
 end do

 ! c3
 allocate(character(1) :: c3(12))
 do i = 1,12
   c3(i) = CHAR(i+64) 
 end do

 ! c33
 allocate(c33(12))
 do i = 1,12
   c33(i) = CHAR(i+64) 
 end do

 ! c4
 do i = 1, 30
   t4(i) = CHAR(i+64) 
 end do

 c4 => t4

 allocate(character(1) :: c6(4,3))
 c6 = RESHAPE(["A","B","C","D","E","F","G","H","I","J","K","L"], [4,3])
 
 !************ calling sub_1 routine with different possible actual arguments **************!

 !!---- actual argument for sub_1 is explicit_shape array
 call sub_1(c1)
 call sub_1(c11)

 !!---- actual argument for sub_1 is defered_shape array  
 call sub_1(c2)  
 call sub_1(c22)
 call sub_1(c3)  
 call sub_1(c33) 
 call sub_1(c4)  

 !!---- actual argument for sub_1 is implied_shape array
 call sub_1(c5)

 !!---- actual argument for sub_1 is an expression array
 call sub_1(c1 // "")
 
 call sub_int1(c1)

 call sub_int2(c11) 

 call sub_3(c6)  

contains 
  subroutine sub_int1(arg2)
    character(1) :: arg2(2,2,*)
 !!---- actual argument for sub_1 is an assumed_size array 
    call sub_1(arg2)
  end subroutine 
  subroutine sub_int2(arg3)
    character(1):: arg3(1:,:,:)
 !!---- actual argument for sub_1 is an assumed_shape array
    call sub_1(arg3)  
  end subroutine

end program  

subroutine sub_3(arg) bind(c)
  character(:), allocatable :: arg(:,:,:)
  interface
   subroutine sub_1(arg1) bind(c)
    character(*) :: arg1(2,2,3)
   end subroutine
  end interface 
 !!---- actual argument for sub_1 is a CFI descriptor
  call sub_1(arg)
end subroutine



