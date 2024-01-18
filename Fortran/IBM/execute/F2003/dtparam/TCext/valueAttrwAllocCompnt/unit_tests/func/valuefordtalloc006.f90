! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/valueAttrwAllocCompnt/unit_tests/func/valuefordtalloc006.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=self

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: valuefordtalloc006.f
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
!*  TEST CASE TITLE            : valuefordtalloc006
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
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : functional testing of value attribute
!*                               for derived types with allocatable
!*                               components (deep copy of derived type)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  module m 
     type A(n1,k1)    ! (20,4)
      integer, kind            :: k1
      integer, len             :: n1
      integer(k1), allocatable :: x(:)
    end type
    
    type B(n2,k2)    ! (20,4)
      integer, kind              :: k2
      integer, len               :: n2
      type(A(:,k2)), allocatable :: A1
      integer(k2)                :: y
    end type
    
    type C(n3,k3)    ! (20,4)
      integer, kind              :: k3
      integer, len               :: n3
      integer(k3)                :: z
      type(B(:,k3)), allocatable :: B1
    end type
  end module
  
  subroutine sub(C2)
    use m
    type(C(20,4)) :: C2
    value :: C2
    integer :: i
    
    do i=1,5
      if(C2%B1%A1%x(i).ne.i) error stop 1
    end do
    
    if(C2%z .ne. 1 .or. C2%B1%y .ne. 5) error stop 2
    
    do i=1,5
      C2%B1%A1%x(i)=0
    end do
    
    C2%z=0
    C2%B1%y=0

  end subroutine
  
   use m
  
  type(C(20,4)) :: C1
  integer :: i
  
  interface
    subroutine sub(C2)
    use m
    type(C(20,4)) :: C2
    value :: C2
    end subroutine
  end interface
  
  C1%z=1
  allocate(B(20,4) :: C1%B1)
  C1%B1%y=5
  allocate(A(20,4) :: C1%B1%A1)
  allocate(C1%B1%A1%x(5))
  
  do i=1,5
    C1%B1%A1%x(i)=i
  end do
  
  call sub(C1)
  
  do i=1,5
      if(C1%B1%A1%x(i).ne.i) error stop 3
  end do
  
  if(C1%z .ne. 1 .or. C1%B1%y .ne. 5) error stop 4

end program
