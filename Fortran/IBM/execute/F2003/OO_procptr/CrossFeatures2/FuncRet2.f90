! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 26, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Function Return - array
!*  (314988)
!*  327080 points out TC problem.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: DT

      INTEGER(1), ALLOCATABLE :: I1Arr(:)
      INTEGER(2), ALLOCATABLE :: I2Arr(:)
      INTEGER(4), ALLOCATABLE :: I4Arr(:)
      INTEGER(8), ALLOCATABLE :: I8Arr(:)

      REAL(4),     ALLOCATABLE  :: R4Arr(:)
      REAL(8),     ALLOCATABLE  :: R8Arr(:)
      REAL(16),    ALLOCATABLE  :: R16Arr(:)

      COMPLEX(8),  ALLOCATABLE :: C8Arr(:)
      COMPLEX(16), ALLOCATABLE :: C16Arr(:)

      LOGICAL(1),  ALLOCATABLE :: L1Arr(:)
      LOGICAL(2),  ALLOCATABLE :: L2Arr(:)
      LOGICAL(4),  ALLOCATABLE :: L4Arr(:)
      LOGICAL(8),  ALLOCATABLE :: L8Arr(:)

      CHARACTER(3)  ::CharArr(1)

      PROCEDURE(ModFun), PASS, POINTER :: ProcPtr=>NULL()
      CONTAINS
      PROCEDURE, PASS :: Proc => ModFun

    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      TYPE(DT), ALLOCATABLE :: T1
      TYPE(DT), POINTER     :: T2
    END TYPE

  CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT) :: Arg
    CLASS(*), POINTER :: ModFun
      ALLOCATE(ModFun, SOURCE=Arg)
    END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT1) :: Arg(:)
  !CLASS(*), POINTER :: ExtFun(:)   ! --> too many calls to this lead to huge mem leak.
  CLASS(*), ALLOCATABLE :: ExtFun(:)
    ALLOCATE(ExtFun(SIZE(Arg)), SOURCE=Arg)
  END FUNCTION

  PROGRAM FuncRet2
  USE M
  IMPLICIT TYPE(DT1)(P)

  INTERFACE
    FUNCTION Fun(Arg)
      IMPORT DT1
      CLASS(*), POINTER :: Fun(:)
      TYPE(DT1)         :: Arg(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(Fun)                  :: ExtFun
  PROCEDURE(Fun),         POINTER :: ProcPtr1
  PROCEDURE(ExtFun),      POINTER :: ProcPtr2
  PROCEDURE(ProcPtr2),    POINTER :: ProcPtr3

  TYPE(DT1)        :: Arr(10000)
  TYPE(DT), TARGET :: Tar
  TYPE(DT)         ::  Const

  type container
    class(*), pointer :: x(:)
  end type

  type(container) :: d1, d2

  Const = DT(                                        &
  &                                                  &
  &                            (/-1_1/),             &
  &                            (/-2_1/),             &
  &                            (/-4_1/),             &
  &                            (/-8_1/),             &
  &                                                  &
  &                            (/-4.0_4/),           &
  &                            (/-8.0_8/),           &
  &                            (/-16.0_16/),         &
  &                                                  &
  &                            (/(4.0_4,-4.0_4)/),   &
  &                            (/(8.0_8,-8.0_8)/),   &
  &                                                  &
  &                            (/.TRUE._1/),         &
  &                            (/.TRUE._2/),         &
  &                            (/.TRUE._4/),         &
  &                            (/.TRUE._8/),         &
  &                                                  &
  &                            (/"abc"/), NULL()     )


  Const%ProcPtr => ModFun
  Tar = Const
  Arr = DT1(DT=Const, T1=Const, T2=Tar)

  CALL IntSub( ExtFun(Arr), Arr)

  ProcPtr1 => ExtFun
  CALL IntSub( ProcPtr1(Arr),Arr )

  ProcPtr2 => ProcPtr1
  CALL IntSub( ProcPtr2(Arr),Arr )

  ProcPtr3 => ProcPtr2
  CALL IntSub( ProcPtr3(Arr),Arr )

  call composeArray1 (d1, arr)
  call composeArray2 (d2, arr)
!  DO I=1, 10000
!    CALL IntSub( (/(Arr(J)%ProcPtr(), J=1,10000)/),Arr )
  call intsub (d1%x, arr)  !<-- replace the previous AC-IMPDO
!    CALL IntSub( (/(Arr(J)%Proc(), J=1,10000)/),Arr )
  call intsub (d2%x, arr)  !<-- replace the previous AC-IMPDO
!  END DO

  CONTAINS

  subroutine composeArray2 (co1, t1)
    type(container), intent(inout) :: co1
    type(dt1), intent(in) :: t1(:)

    class (*), pointer :: local

    allocate (dt1 :: co1%x(size(t1)))
    !! make a deep copy of t1(:) using dt%proc()

    select type (x => co1%x)
        type is (dt1)
            do i = 1, size(t1)
                local => t1(i)%proc()
                select type (y => local)
                    type is (dt1)
                        x(i) = y
                    class default
                        stop 117
                end select

                deallocate (local)
            end do
        class default
            stop 118
    end select
  end subroutine

  subroutine composeArray1 (co1, t1)
    type(container), intent(inout) :: co1
    type(dt1), intent(in) :: t1(:)

    class (*), pointer :: local

    allocate (dt1 :: co1%x(size(t1)))
    !! make a deep copy of t1(:) using dt%procptr()

    select type (x => co1%x)
        type is (dt1)
            do i = 1, size(t1)
                local => t1(i)%procptr()
                select type (y => local)
                    type is (dt1)
                        x(i) = y
                    class default
                        stop 119
                end select

                deallocate (local)
            end do
        class default
            stop 120
    end select
  end subroutine

  SUBROUTINE IntSub(Arg1, Arg2)
  CLASS(*)  :: Arg1(:)
  TYPE(DT1) :: Arg2(:)
  INTEGER   :: I

    DO I=1, SIZE(Arg1)
    SELECT TYPE(Arg1)
    CLASS IS (DT1)

      IF (ANY(Arg1(I)%T1%I1Arr .NE. Arg2(I)%T1%I1Arr)) STOP 11
      IF (ANY(Arg1(I)%T1%I2Arr .NE. Arg2(I)%T1%I2Arr)) STOP 12
      IF (ANY(Arg1(I)%T1%I4Arr .NE. Arg2(I)%T1%I4Arr)) STOP 14
      IF (ANY(Arg1(I)%T1%I8Arr .NE. Arg2(I)%T1%I8Arr)) STOP 18

      IF (ANY(Arg1(I)%T1%R4Arr  .NE. Arg2(I)%T1%R4Arr))  STOP 24
      IF (ANY(Arg1(I)%T1%R8Arr  .NE. Arg2(I)%T1%R8Arr))  STOP 28
      IF (ANY(Arg1(I)%T1%R16Arr .NE. Arg2(I)%T1%R16Arr)) STOP 26

      IF (ANY(Arg1(I)%T1%C8Arr  .NE. Arg2(I)%T1%C8Arr))  STOP 38
      IF (ANY(Arg1(I)%T1%C16Arr .NE. Arg2(I)%T1%C16Arr)) STOP 36

      IF (ANY(Arg1(I)%T1%L1Arr .NEQV. Arg2(I)%T1%L1Arr)) STOP 41
      IF (ANY(Arg1(I)%T1%L2Arr .NEQV. Arg2(I)%T1%L2Arr)) STOP 42
      IF (ANY(Arg1(I)%T1%L4Arr .NEQV. Arg2(I)%T1%L4Arr)) STOP 44
      IF (ANY(Arg1(I)%T1%L8Arr .NEQV. Arg2(I)%T1%L8Arr)) STOP 48

      IF (ANY(Arg1(I)%T1%CharArr .NE. Arg2(I)%T1%CharArr)) STOP 68

    CLASS DEFAULT
      STOP 69
    END SELECT
    END DO

    DO I=1, SIZE(Arg1)
    SELECT TYPE(Arg1)
    CLASS IS (DT1)

      IF (ANY(Arg1(I)%T2%I1Arr .NE. Arg2(I)%T2%I1Arr)) STOP 11
      IF (ANY(Arg1(I)%T2%I2Arr .NE. Arg2(I)%T2%I2Arr)) STOP 12
      IF (ANY(Arg1(I)%T2%I4Arr .NE. Arg2(I)%T2%I4Arr)) STOP 14
      IF (ANY(Arg1(I)%T2%I8Arr .NE. Arg2(I)%T2%I8Arr)) STOP 18

      IF (ANY(Arg1(I)%T2%R4Arr  .NE. Arg2(I)%T2%R4Arr))  STOP 24
      IF (ANY(Arg1(I)%T2%R8Arr  .NE. Arg2(I)%T2%R8Arr))  STOP 28
      IF (ANY(Arg1(I)%T2%R16Arr .NE. Arg2(I)%T2%R16Arr)) STOP 26

      IF (ANY(Arg1(I)%T2%C8Arr  .NE. Arg2(I)%T2%C8Arr))  STOP 38
      IF (ANY(Arg1(I)%T2%C16Arr .NE. Arg2(I)%T2%C16Arr)) STOP 36

      IF (ANY(Arg1(I)%T2%L1Arr .NEQV. Arg2(I)%T2%L1Arr)) STOP 41
      IF (ANY(Arg1(I)%T2%L2Arr .NEQV. Arg2(I)%T2%L2Arr)) STOP 42
      IF (ANY(Arg1(I)%T2%L4Arr .NEQV. Arg2(I)%T2%L4Arr)) STOP 44
      IF (ANY(Arg1(I)%T2%L8Arr .NEQV. Arg2(I)%T2%L8Arr)) STOP 48

      IF (ANY(Arg1(I)%T2%CharArr .NE. Arg2(I)%T2%CharArr)) STOP 68

    CLASS DEFAULT
      STOP 69
    END SELECT
    END DO

  END SUBROUTINE

  END

