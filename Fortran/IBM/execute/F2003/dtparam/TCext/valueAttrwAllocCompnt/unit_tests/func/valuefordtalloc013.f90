! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/unit_tests/func/valuefordtalloc013.f
! opt variations: -qnol -qreuse=self -qreuse=none

!**********************************************************************
! %STBRT
! %MBIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: valuefordtalloc013.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECBRGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          
!*  ===================================================================
!*
!*  TEST CBSE TITLE            : valuefordtalloc013
!*
!*  PROGRBMMER                 : Michael Selvanayagam
!*  DBTE                       : Jan, 20, 2006
!*  ORIGIN                     : BIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMBRY FUNCTIONS TESTED   : value attribute for derived types with allocatable components
!*  SECONDBRY FUNCTIONS TESTED : None
!*
!*  DRIVER STBNZB              : xlf95
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=polymorphic
!*
!*  DESCRIPTION                : functional testing of value attribute
!*                               for derived types with allocatable
!*                               components
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  module m
    type A(n1,k1,k2)    ! (20,4,4)
      integer, kind            :: k1,k2
      integer, len             :: n1
      integer(k1)              :: x
      integer(k2), allocatable :: y             
    end type
    
    type, extends(A) :: B    ! (20,4,4)
      integer(k1), allocatable :: z
      
      contains
        
        procedure, nopass ::  sub1 => sub
        
    end type
    
    contains
      
      subroutine sub(B1, B2)
        type(B(20,4,4)), value :: B1
        type(B(20,4,4)), value :: B2
        
        if((B1%x .ne. 1) .or. (B1%y .ne. 2) .or. (B1%z  .ne. 3)) error stop 1
        if((B2%x .ne. 4) .or. (B2%y .ne. 5) .or. (B2%z  .ne. 6)) error stop 2
         
        B1%x=0
        B1%y=0
        B1%z=0
        
        B2%x=0
        B2%y=0
        B2%z=0

      end subroutine
    
  end module
  
  use m
  
  type(B(20,4,4)) :: B3, B4
  
  B3%x=1
  allocate(B3%y)
  allocate(B3%z)
  B3%y=2
  B3%z=3
  
  B4%x=4
  allocate(B4%y)
  allocate(B4%z)
  B4%y=5
  B4%z=6
  
  call B3%sub1(b3, B4)

  
  if((B3%x .ne. 1) .or. (B3%y .ne. 2) .or. (B3%z  .ne. 3)) error stop 3
  if((B4%x .ne. 4) .or. (B4%y .ne. 5) .or. (B4%z  .ne. 6)) error stop 4

end program


  
  
  
  
  
