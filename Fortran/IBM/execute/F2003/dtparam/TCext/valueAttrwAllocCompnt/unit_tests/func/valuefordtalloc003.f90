! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/unit_tests/func/valuefordtalloc003.f
! opt variations: -qnol -qreuse=none

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: valuefordtalloc003.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valuefordtalloc003
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : Jan, 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : value attribute for derived types with allocatable components
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : functional testing of value attribute
!*                               for derived types with allocatable
!*                               components
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type A(n1,k1)    ! (20,4)
    integer, kind         :: k1
    integer, len          :: n1
    real(k1), allocatable :: x(:)
    integer(k1)           :: y
    real(k1)              :: z
  end type
  
  type(A(20,4)) :: A1
  integer :: i
  
  A1%y=5
  A1%z=1.0
  
  i=func(A1)
  if(allocated(A1%x)) error stop 1
  
   contains
     function func(A2)
       type(A(20,4)), value :: A2
       integer :: i, func
       
       if(allocated(A2%x)) error stop 2
       allocate(A2%x(5))
       
       do i=1,5
         A2%x(i)=A2%z 
       end do
       
       func=0

     end function

end program
