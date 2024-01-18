!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: valuefordtalloc011.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valuefordtalloc011
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : Jan, 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : value attribute for derived types with allocatable components
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=polymorphic
!*
!*  DESCRIPTION                : functional testing of value attribute
!*                               for derived types with allocatable
!*                               components
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  module m
    type A
      integer :: x
      integer, allocatable :: y
      integer, allocatable :: z
      
      contains
        
        procedure :: sub1
        procedure, pass(a2) :: sub2
        procedure, nopass :: sub3
        generic, public :: sub => sub1, sub2, sub3
          
    end type
    
    contains
      
      subroutine sub1(A1, A2,num)
        class(A) :: A1
        type(A) :: A2
        real :: num
        value :: A2
        
        if((A1%x .ne. 1) .or. (A1%y .ne. 2) .or. (A1%z  .ne. 3)) error stop 11_4
        if((A2%x .ne. 4) .or. (A2%y .ne. 5) .or. (A2%z  .ne. 6)) error stop 12_4
         
        A1%x=0
        A1%y=0
        A1%z=0
        
        A2%x=0
        A2%y=0
        A2%z=0

      end subroutine
      
      subroutine sub2(A1, A2, num)
        type(A):: A1
        class(A) :: A2
        integer :: num
        value :: A1
        
        if((A1%x .ne. 1) .or. (A1%y .ne. 2) .or. (A1%z  .ne. 3)) error stop 21_4
        if((A2%x .ne. 4) .or. (A2%y .ne. 5) .or. (A2%z  .ne. 6)) error stop 22_4
         
        A1%x=0
        A1%y=0
        A1%z=0
        
        A2%x=0
        A2%y=0
        A2%z=0

      end subroutine
      
      subroutine sub3(A1, A2, num)
        type(A) :: A1
        type(A) :: A2
        complex :: num
        value :: A1, A2
        
        if((A1%x .ne. 1) .or. (A1%y .ne. 2) .or. (A1%z  .ne. 3)) error stop 31_4
        if((A2%x .ne. 4) .or. (A2%y .ne. 5) .or. (A2%z  .ne. 6)) error stop 41_4
         
        A1%x=0
        A1%y=0
        A1%z=0
        
        A2%x=0
        A2%y=0
        A2%z=0

      end subroutine
    
  end module
  
  use m
  
  type(A) :: A3, A4
  
  A3%x=1
  allocate(A3%y)
  allocate(A3%z)
  A3%y=2
  A3%z=3
  
  A4%x=4
  allocate(A4%y)
  allocate(A4%z)
  A4%y=5
  A4%z=6
  
  call A3%sub(A4,1.0)
  if((A3%x .ne. 0) .or. (A3%y .ne. 0) .or. (A3%z  .ne. 0)) error stop 3_4
  if((A4%x .ne. 4) .or. (A4%y .ne. 5) .or. (A4%z  .ne. 6)) error stop 4_4
  
  A3%x=4
  A3%y=5
  A3%z=6
  
  A4%x=1
  A4%y=2
  A4%z=3
  
  call A3%sub(A4,1)
  if((A4%x .ne. 1) .or. (A4%y .ne. 2) .or. (A4%z  .ne. 3)) error stop 5_4
  if((A3%x .ne. 0) .or. (A3%y .ne. 0) .or. (A3%z  .ne. 0)) error stop 6_4
  
  
  A3%x=1
  A3%y=2
  A3%z=3

  A4%x=4
  A4%y=5
  A4%z=6
  
  call A3%sub(a3, A4,(1.0,1.0))
  if((A3%x .ne. 1) .or. (A3%y .ne. 2) .or. (A3%z  .ne. 3)) error stop 7_4
  if((A4%x .ne. 4) .or. (A4%y .ne. 5) .or. (A4%z  .ne. 6)) error stop 8_4

end program


  
  
  
  
 
