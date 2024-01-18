! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/unit_tests/func/valuefordtalloc009.f
! opt variations: -ql -qreuse=none

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: valuefordtalloc009.f
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
!*  TEST CASE TITLE            : valuefordtalloc009
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
    type A(k1)    ! (4)
      integer, kind            :: k1
      integer(k1)              :: x
      integer(k1), allocatable :: y
      integer(k1), allocatable :: z
      
      contains
        
        procedure, pass(a2) ::  sub1 => sub
          
    end type
    
    contains
      
      subroutine sub(A1, A2)
        class(A(4)) :: A2
        type(A(4)), value :: A1
        
        if((A2%x .ne. 1) .or. (A2%y .ne. 2) .or. (A2%z  .ne. 3)) error stop 1_4
        if((A1%x .ne. 4) .or. (A1%y .ne. 5) .or. (A1%z  .ne. 6)) error stop 2_4
         
        A1%x=0
        A1%y=0
        A1%z=0
        
        A2%x=0
        A2%y=0
        A2%z=0

      end subroutine
    
  end module
  
  use m
  
  type(A(4)) :: A3, A4
  
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
  
  call A3%sub1(A4)

  
  if((A4%x .ne. 4) .or. (A4%y .ne. 5) .or. (A4%z  .ne. 6)) error stop 3_4
  if((A3%x .ne. 0) .or. (A3%y .ne. 0) .or. (A3%z  .ne. 0)) error stop 4_4

end program


  
  
  
  
  
