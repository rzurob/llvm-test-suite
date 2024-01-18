! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/Misc/Misc29.f
! opt variations: -qnok -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Misc29.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc29
!*
!*  DATE                       : Mar. 17, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Limits on TYPE IS/CLASS IS
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc29

  TYPE :: DT0(K1)    ! (4)
      INTEGER, KIND :: K1
  END TYPE

  TYPE, EXTENDS(DT0) :: DT1    ! (4)
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2    ! (4)
  END TYPE

  TYPE, EXTENDS(DT2) :: DT3    ! (4)
  END TYPE

  TYPE, EXTENDS(DT3) :: DT4    ! (4)
  END TYPE

  TYPE, EXTENDS(DT4) :: DT5    ! (4)
  END TYPE

  TYPE, EXTENDS(DT5) :: DT6    ! (4)
  END TYPE

  TYPE, EXTENDS(DT6) :: DT7    ! (4)
  END TYPE

  TYPE, EXTENDS(DT7) :: DT8    ! (4)
  END TYPE

  TYPE, EXTENDS(DT8) :: DT9    ! (4)
  END TYPE

  TYPE, EXTENDS(DT9) :: DT10    ! (4)
  END TYPE

  TYPE, EXTENDS(DT10) :: DT11    ! (4)
  END TYPE

  TYPE, EXTENDS(DT11) :: DT12    ! (4)
  END TYPE

  TYPE, EXTENDS(DT12) :: DT13    ! (4)
  END TYPE

  TYPE, EXTENDS(DT13) :: DT14    ! (4)
  END TYPE

  TYPE, EXTENDS(DT14) :: DT15    ! (4)
  END TYPE

  TYPE, EXTENDS(DT15) :: DT16    ! (4)
  END TYPE

  TYPE, EXTENDS(DT16) :: DT17    ! (4)
  END TYPE

  TYPE, EXTENDS(DT17) :: DT18    ! (4)
  END TYPE

  TYPE, EXTENDS(DT18) :: DT19    ! (4)
  END TYPE

  TYPE, EXTENDS(DT19) :: DT20    ! (4)
  END TYPE

  TYPE, EXTENDS(DT20) :: DT21    ! (4)
  END TYPE

  TYPE, EXTENDS(DT21) :: DT22    ! (4)
  END TYPE

  TYPE, EXTENDS(DT22) :: DT23    ! (4)
  END TYPE

  TYPE, EXTENDS(DT23) :: DT24    ! (4)
  END TYPE

  TYPE, EXTENDS(DT24) :: DT25    ! (4)
  END TYPE

  TYPE, EXTENDS(DT25) :: DT26    ! (4)
  END TYPE

  TYPE, EXTENDS(DT26) :: DT27    ! (4)
  END TYPE

  TYPE, EXTENDS(DT27) :: DT28    ! (4)
  END TYPE

  TYPE, EXTENDS(DT28) :: DT29    ! (4)
  END TYPE

  TYPE, EXTENDS(DT29) :: DT30    ! (4)
  END TYPE


  TYPE, EXTENDS(DT30) :: DT31    ! (4)
  END TYPE

  TYPE, EXTENDS(DT31) :: DT32    ! (4)
  END TYPE

  TYPE, EXTENDS(DT32) :: DT33    ! (4)
  END TYPE

  TYPE, EXTENDS(DT33) :: DT34    ! (4)
  END TYPE

  TYPE, EXTENDS(DT34) :: DT35    ! (4)
  END TYPE

  TYPE, EXTENDS(DT35) :: DT36    ! (4)
  END TYPE

  TYPE, EXTENDS(DT36) :: DT37    ! (4)
  END TYPE

  TYPE, EXTENDS(DT37) :: DT38    ! (4)
  END TYPE

  TYPE, EXTENDS(DT38) :: DT39    ! (4)
  END TYPE

  TYPE, EXTENDS(DT39) :: DT40    ! (4)
  END TYPE


  TYPE, EXTENDS(DT40) :: DT41    ! (4)
  END TYPE

  TYPE, EXTENDS(DT41) :: DT42    ! (4)
  END TYPE

  TYPE, EXTENDS(DT42) :: DT43    ! (4)
  END TYPE

  TYPE, EXTENDS(DT43) :: DT44    ! (4)
  END TYPE

  TYPE, EXTENDS(DT44) :: DT45    ! (4)
  END TYPE

  TYPE, EXTENDS(DT45) :: DT46    ! (4)
  END TYPE

  TYPE, EXTENDS(DT46) :: DT47    ! (4)
  END TYPE

  TYPE, EXTENDS(DT47) :: DT48    ! (4)
  END TYPE

  TYPE, EXTENDS(DT48) :: DT49    ! (4)
  END TYPE

  TYPE, EXTENDS(DT49) :: DT50    ! (4)
  END TYPE

  ! When the DT?? goes further,
  ! 1517-006 (U) Parse stack overflow.  Expression is too long.
  !  Reduce program size or nested references

  CLASS(*), ALLOCATABLE :: V

  ALLOCATE(DT50(4)::V)

  SELECT TYPE (V)

    CLASS DEFAULT
      STOP 99

    TYPE IS  (DT1(4))
      STOP 1
    CLASS IS (DT1(4))
      STOP 1
    TYPE IS  (DT2(4))
      STOP 2
    CLASS IS (DT2(4))
      STOP 2
    TYPE IS  (DT3(4))
      STOP 3
    CLASS IS (DT3(4))
      STOP 3
    TYPE IS  (DT4(4))
      STOP 4
    CLASS IS (DT4(4))
      STOP 4
    TYPE IS  (DT5(4))
      STOP 5
    CLASS IS (DT5(4))
      STOP 5
    TYPE IS  (DT6(4))
      STOP 6
    CLASS IS (DT6(4))
      STOP 6
    TYPE IS  (DT7(4))
      STOP 7
    CLASS IS (DT7(4))
      STOP 7
    TYPE IS  (DT8(4))
      STOP 8
    CLASS IS (DT8(4))
      STOP 8
    TYPE IS  (DT9(4))
      STOP 9
    CLASS IS (DT9(4))
      STOP 9
    TYPE IS  (DT10(4))
      STOP 10
    CLASS IS (DT10(4))
      STOP 10

    TYPE IS  (DT11(4))
      STOP 11
    CLASS IS (DT11(4))
      STOP 11
    TYPE IS  (DT12(4))
      STOP 12
    CLASS IS (DT12(4))
      STOP 12
    TYPE IS  (DT13(4))
      STOP 13
    CLASS IS (DT13(4))
      STOP 13
    TYPE IS  (DT14(4))
      STOP 14
    CLASS IS (DT14(4))
      STOP 14
    TYPE IS  (DT15(4))
      STOP 15
    CLASS IS (DT15(4))
      STOP 15
    TYPE IS  (DT16(4))
      STOP 16
    CLASS IS (DT16(4))
      STOP 16
    TYPE IS  (DT17(4))
      STOP 17
    CLASS IS (DT17(4))
      STOP 17
    TYPE IS  (DT18(4))
      STOP 18
    CLASS IS (DT18(4))
      STOP 18
    TYPE IS  (DT19(4))
      STOP 19
    CLASS IS (DT19(4))
      STOP 19
    TYPE IS  (DT20(4))
      STOP 20
    CLASS IS (DT20(4))
      STOP 20

    TYPE IS  (DT21(4))
      STOP 21
    CLASS IS (DT21(4))
      STOP 21
    TYPE IS  (DT22(4))
      STOP 22
    CLASS IS (DT22(4))
      STOP 22
    TYPE IS  (DT23(4))
      STOP 23
    CLASS IS (DT23(4))
      STOP 23
    TYPE IS  (DT24(4))
      STOP 24
    CLASS IS (DT24(4))
      STOP 24
    TYPE IS  (DT25(4))
      STOP 25
    CLASS IS (DT25(4))
      STOP 25
    TYPE IS  (DT26(4))
      STOP 26
    CLASS IS (DT26(4))
      STOP 26
    TYPE IS  (DT27(4))
      STOP 27
    CLASS IS (DT27(4))
      STOP 27
    TYPE IS  (DT28(4))
      STOP 28
    CLASS IS (DT28(4))
      STOP 28
    TYPE IS  (DT29(4))
      STOP 29
    CLASS IS (DT29(4))
      STOP 29
    TYPE IS  (DT30(4))
      STOP 30
    CLASS IS (DT30(4))
      STOP 30

    TYPE IS  (DT31(4))
      STOP 31
    CLASS IS (DT31(4))
      STOP 31
    TYPE IS  (DT32(4))
      STOP 32
    CLASS IS (DT32(4))
      STOP 32
    TYPE IS  (DT33(4))
      STOP 33
    CLASS IS (DT33(4))
      STOP 33
    TYPE IS  (DT34(4))
      STOP 34
    CLASS IS (DT34(4))
      STOP 34
    TYPE IS  (DT35(4))
      STOP 35
    CLASS IS (DT35(4))
      STOP 35
    TYPE IS  (DT36(4))
      STOP 36
    CLASS IS (DT36(4))
      STOP 36
    TYPE IS  (DT37(4))
      STOP 37
    CLASS IS (DT37(4))
      STOP 37
    TYPE IS  (DT38(4))
      STOP 38
    CLASS IS (DT38(4))
      STOP 38
    TYPE IS  (DT39(4))
      STOP 39
    CLASS IS (DT39(4))
      STOP 39
    TYPE IS  (DT40(4))
      STOP 40
    CLASS IS (DT40(4))
      STOP 40

    TYPE IS  (DT41(4))
      STOP 41
    CLASS IS (DT41(4))
      STOP 41
    TYPE IS  (DT42(4))
      STOP 42
    CLASS IS (DT42(4))
      STOP 42
    TYPE IS  (DT43(4))
      STOP 43
    CLASS IS (DT43(4))
      STOP 43
    TYPE IS  (DT44(4))
      STOP 44
    CLASS IS (DT44(4))
      STOP 44
    TYPE IS  (DT45(4))
      STOP 45
    CLASS IS (DT45(4))
      STOP 45
    TYPE IS  (DT46(4))
      STOP 46
    CLASS IS (DT46(4))
      STOP 46
    TYPE IS  (DT47(4))
      STOP 47
    CLASS IS (DT47(4))
      STOP 47
    TYPE IS  (DT48(4))
      STOP 48
    CLASS IS (DT48(4))
      STOP 48
    TYPE IS  (DT49(4))
      STOP 49
    CLASS IS (DT49(4))
      STOP 49
    TYPE IS  (DT50(4))
      PRINT *,  "OK!"
    CLASS IS (DT50(4))
      STOP 50

  END SELECT


  END


