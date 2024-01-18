!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : d374918
!*
!*  DATE                       : 2010-02-04
!*
!*  PRIMARY FUNCTIONS TESTED   : misc
!*
!*  SECONDARY FUNCTIONS TESTED : catch defects 374918 (default init not applied) and 374920 (BADC0FEE length parameter)
!*
!*  REFERENCE                  : defect 367675
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAABasicFunction003 (dtpIAABasic003<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  As part of an attempted fix to unexpected output from dtpIAABasicFunction003 (ultimately defect 367675),
!*  this code shows that default initialisation is not done completely, and
!*  the length parameter associated with the object returned from a function
!*  is somehow tagged with an "ignore" flag.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod

  implicit none

  type dk(k)
     integer, kind :: k
     integer(k) :: ivar = 0
  end type dk

  type dl(l)
     integer, len :: l
     character(l) :: chvar = ''
     integer :: iarr(l) = 0
  end type dl

  type container(k,l)
     integer, kind :: k
     integer, len  :: l
     type(dk(k)) :: dkvar = dk(k)()
     type(dl(l)) :: dlvar
   contains
     final :: fin1
  end type container


contains

  subroutine fin1(a)
    type(container(1,*)) :: a
    print *, "fin1:", a%l, a%dkvar, a%dlvar
  end subroutine fin1

  function func1(l,i,str,iarr)
    integer, parameter :: k = 1
    type(container(k,l)) :: func1
    integer :: l, i, iarr(*), j
    character(*) :: str
    print *, "in func1:", l, i, str, (iarr(j), j=1,l), func1%l
    func1%dkvar%ivar = 9 ! mark the value associated with the function name
    func1 = container(k,l)(dk(k)(i), dl(l)(str,iarr))
    print *, 'returning from func1'
  end function func1


end module mod

program d374918

  use mod
  implicit none

  type(container(1,2)) :: o1

  o1 = func1(2,34,'ab',[35,36])
  print *, 'done'

end program d374918
