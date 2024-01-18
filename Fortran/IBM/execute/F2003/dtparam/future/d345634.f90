
  PROGRAM dtParamTypeDecC501_63

  TYPE :: DT0(L)
    INTEGER, LEN  :: L=1
  END TYPE

  TYPE(DT0(6)), PARAMETER :: T0=DT0(6)()

  TYPE, EXTENDS(DT0) :: DT
  !TYPE  :: DT  ! <-- this works
    integer :: I=T0%L
  END TYPE

  TYPE (DT) :: T1

  print*, T0%L
  print*, T1%I
  IF ( T1%I    .NE.   T0%L  ) STOP 51

  END
