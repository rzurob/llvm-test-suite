!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 17, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  The basic syatax
!*  CHARACTER
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecIntrinsic2

  CHARACTER(LEN=10)             :: C1(1)   = CHAR(1)
  CHARACTER(LEN=:), POINTER     :: C2(:)
  CHARACTER(LEN=:), ALLOCATABLE :: C3(:)

  CHARACTER*(10)             :: C4(1)      = CHAR(1)
  CHARACTER*(:), POINTER     :: C5(:)
  CHARACTER*(:), ALLOCATABLE :: C6(:)

  CHARACTER(LEN=10, KIND=1)              :: C7(1)   = CHAR(1)
  CHARACTER(LEN=:,  KIND=1), POINTER     :: C8(:)
  CHARACTER(LEN=:,  KIND=1), ALLOCATABLE :: C9(:)


  IF ( C1%KIND            .NE.   1          ) ERROR STOP 11
  IF ( C1%LEN             .NE.   10         ) ERROR STOP 12
  IF ( ANY( C1            .NE.   CHAR(1) )  ) ERROR STOP 13

  ALLOCATE( C2(10), SOURCE="1234567890")
  ALLOCATE( C3(10), SOURCE="1234567890")
  IF ( C2%KIND            .NE.   1               ) ERROR STOP 14
  IF ( C2%LEN             .NE.   10              ) ERROR STOP 15
  IF ( ANY( C2            .NE.   "1234567890" )  ) ERROR STOP 16
  IF ( C3%KIND            .NE.   1               ) ERROR STOP 17
  IF ( C3%LEN             .NE.   10              ) ERROR STOP 19
  IF ( ANY( C3            .NE.   "1234567890" )  ) ERROR STOP 20

  CALL IntSub1( C2, SIZE(C2) )
  CALL IntSub1( C3, SIZE(C3) )


  IF ( C4%KIND            .NE.   1          ) ERROR STOP 31
  IF ( C4%LEN             .NE.   10         ) ERROR STOP 32
  IF ( ANY( C4            .NE.   CHAR(1) )  ) ERROR STOP 33

  ALLOCATE( C5(10), SOURCE="1234567890")
  ALLOCATE( C6(10), SOURCE="1234567890")
  IF ( C5%KIND            .NE.   1               ) ERROR STOP 34
  IF ( C5%LEN             .NE.   10              ) ERROR STOP 35
  IF ( ANY( C5            .NE.   "1234567890" )  ) ERROR STOP 36
  IF ( C6%KIND            .NE.   1               ) ERROR STOP 37
  IF ( C6%LEN             .NE.   10              ) ERROR STOP 39
  IF ( ANY( C6            .NE.   "1234567890" )  ) ERROR STOP 40

  CALL IntSub2( C5, SIZE(C5) )
  CALL IntSub2( C6, SIZE(C6) )


  IF ( C7%KIND            .NE.   1          ) ERROR STOP 51
  IF ( C7%LEN             .NE.   10         ) ERROR STOP 52
  IF ( ANY( C7            .NE.   CHAR(1) )  ) ERROR STOP 53

  ALLOCATE( C8(10), SOURCE="1234567890")
  ALLOCATE( C9(10), SOURCE="1234567890")
  IF ( C8%KIND            .NE.   1               ) ERROR STOP 54
  IF ( C8%LEN             .NE.   10              ) ERROR STOP 55
  IF ( ANY( C8            .NE.   "1234567890" )  ) ERROR STOP 56
  IF ( C9%KIND            .NE.   1               ) ERROR STOP 57
  IF ( C9%LEN             .NE.   10              ) ERROR STOP 59
  IF ( ANY( C9            .NE.   "1234567890" )  ) ERROR STOP 60

  CALL IntSub3( C8, SIZE(C8) )
  CALL IntSub3( C9, SIZE(C9) )


  CONTAINS

  SUBROUTINE IntSub1(C4, Len)
  INTEGER          :: Len
  CHARACTER(LEN=*) :: C4(Len)

  IF ( C4%KIND            .NE.   1               ) ERROR STOP 21
  IF ( C4%LEN             .NE.   10              ) ERROR STOP 22
  IF ( SIZE( C4 )         .NE.   Len             ) ERROR STOP 23
  IF ( ANY( C4            .NE.   "1234567890" )  ) ERROR STOP 24

  END SUBROUTINE

  SUBROUTINE IntSub2(C7, Len)
  INTEGER          :: Len
  CHARACTER*(*) :: C7(Len)

  IF ( C7%KIND            .NE.   1               ) ERROR STOP 41
  IF ( C7%LEN             .NE.   10              ) ERROR STOP 42
  IF ( SIZE( C7 )         .NE.   Len             ) ERROR STOP 43
  IF ( ANY( C7            .NE.   "1234567890" )  ) ERROR STOP 44

  END SUBROUTINE

  SUBROUTINE IntSub3(C11, Len)
  INTEGER          :: Len
  CHARACTER(LEN=*, KIND=1) :: C11(Len)

  IF ( C11%KIND            .NE.   1               ) ERROR STOP 61
  IF ( C11%LEN             .NE.   10              ) ERROR STOP 62
  IF ( SIZE( C11 )         .NE.   Len             ) ERROR STOP 63
  IF ( ANY( C11            .NE.   "1234567890" )  ) ERROR STOP 64

  END SUBROUTINE

  END

