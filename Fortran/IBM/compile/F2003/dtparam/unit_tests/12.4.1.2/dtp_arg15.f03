!*  ===================================================================
!*
!*  FUNCTIONAL TESTED          : Passed-object dummy argument
!*                               - the len type parameter must be assumed
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT1(K, L)
    integer(kind=1), kind :: k=0
    integer(kind=1), len  :: l=0
    integer(kind=k)       :: kk=K
    integer(kind=k)       :: ll(l, l)=1
    contains
    procedure :: proc => modsub1
  END TYPE

  contains
  subroutine modsub1(arg)
  class(dt1(1,2)) :: arg

  print *, arg%ll

  end subroutine

  end module

  program dtp_arg15
  use M
  type (dt1(1,2))    :: t1

  call t1%proc()

  end

