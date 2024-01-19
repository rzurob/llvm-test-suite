
  MODULE M

  TYPE :: DT(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
    CLASS(*), POINTER :: Ptr(:, :)
  CONTAINS
    PROCEDURE, NOPASS :: Fun => ModFun
  END TYPE

  TYPE :: DT1(K2,N2)    ! (4,20)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
    CLASS(*), POINTER :: Ptr(:)
  CONTAINS
    PROCEDURE, NOPASS :: Fun => ModFun1
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT(4,*)),  TARGET :: Arg(:, :)
  TYPE(DT(4,:)), POINTER :: ModFun(:, :)
    ModFun(LBOUND(Arg,1):,LBOUND(Arg,2):) => Arg
    !ModFun => Arg
  END FUNCTION

  FUNCTION ModFun1(Arg)
  CLASS(DT1(4,*)),  TARGET, INTENT(IN) :: Arg(:)
  TYPE(DT1(4,:)), POINTER :: ModFun1(:)
    ModFun1(LBOUND(Arg,1):UBOUND(Arg,1)-1) => Arg
    !ModFun1 => Arg
  END FUNCTION

  END MODULE
