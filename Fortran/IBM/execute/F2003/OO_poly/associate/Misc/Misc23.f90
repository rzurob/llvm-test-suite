! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 16, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc23

  ASSOCIATE ( &
    & A10 => 1, &
    & A11 => 1, &
    & A12 => 1, &
    & A13 => 1, &
    & A14 => 1, &
    & A15 => 1, &
    & A16 => 1, &
    & A17 => 1, &
    & A18 => 1, &
    & A19 => 1, &
    & A20 => 1, &
    & A21 => 1, &
    & A22 => 1, &
    & A23 => 1, &
    & A24 => 1, &
    & A25 => 1, &
    & A26 => 1, &
    & A27 => 1, &
    & A28 => 1, &
    & A29 => 1, &
    & A30 => 1, &
    & A31 => 1, &
    & A32 => 1, &
    & A33 => 1, &
    & A34 => 1, &
    & A35 => 1, &
    & A36 => 1, &
    & A37 => 1, &
    & A38 => 1, &
    & A39 => 1, &
    & A40 => 1, &
    & A41 => 1, &
    & A42 => 1, &
    & A43 => 1, &
    & A44 => 1, &
    & A45 => 1, &
    & A46 => 1, &
    & A47 => 1, &
    & A48 => 1, &
    & A49 => 1, &
    & A50 => 1, &
    & A51 => 1, &
    & A52 => 1, &
    & A53 => 1, &
    & A54 => 1, &
    & A55 => 1, &
    & A56 => 1, &
    & A57 => 1, &
    & A58 => 1, &
    & A59 => 1, &
    & A60 => 1, &
    & A61 => 1, &
    & A62 => 1, &
    & A63 => 1, &
    & A64 => 1, &
    & A65 => 1, &
    & A66 => 1, &
    & A67 => 1, &
    & A68 => 1, &
    & A69 => 1, &
    & A70 => 1, &
    & A71 => 1, &
    & A72 => 1, &
    & A73 => 1, &
    & A74 => 1, &
    & A75 => 1, &
    & A76 => 1, &
    & A77 => 1, &
    & A78 => 1, &
    & A79 => 1, &
    & A80 => 1, &
    & A81 => 1, &
    & A82 => 1, &
    & A83 => 1, &
    & A84 => 1, &
    & A85 => 1, &
    & A86 => 1, &
    & A87 => 1, &
    & A88 => 1, &
    & A89 => 1, &
    & A90 => 1, &
    & A91 => 1, &
    & A92 => 1, &
    & A93 => 1, &
    & A94 => 1, &
    & A95 => 1, &
    & A96 => 1, &
    & A97 => 1, &
    & A98 => 1, &
    & A99 => 1, &
    & A00 => 1, &
    & A01 => 1, &
    & A02 => 1, &
    & A03 => 1, &
    & A04 => 1, &
    & A05 => 1, &
    & A06 => 1, &
    & A07 => 1, &
    & A08 => 1, &
    & A09 => 1, &
    & B   => 1  &
  & )

    IF (A00 .NE. 1 )  STOP 1
    IF (A01 .Ne. 1 )  STOP 1
    IF (A02 .Ne. 1 )  STOP 1
    IF (A03 .Ne. 1 )  STOP 1
    IF (A04 .Ne. 1 )  STOP 1
    IF (A05 .Ne. 1 )  STOP 1
    IF (A06 .Ne. 1 )  STOP 1
    IF (A07 .Ne. 1 )  STOP 1
    IF (A08 .Ne. 1 )  STOP 1
    IF (A09 .Ne. 1 )  STOP 1

    IF (A10 .Ne. 1 )  STOP 1
    IF (A11 .Ne. 1 )  STOP 1
    IF (A12 .Ne. 1 )  STOP 1
    IF (A13 .Ne. 1 )  STOP 1
    IF (A14 .Ne. 1 )  STOP 1
    IF (A15 .Ne. 1 )  STOP 1
    IF (A16 .Ne. 1 )  STOP 1
    IF (A17 .Ne. 1 )  STOP 1
    IF (A18 .Ne. 1 )  STOP 1
    IF (A19 .Ne. 1 )  STOP 1

    IF (A20 .Ne. 1 )  STOP 1
    IF (A21 .Ne. 1 )  STOP 1
    IF (A22 .Ne. 1 )  STOP 1
    IF (A23 .Ne. 1 )  STOP 1
    IF (A24 .Ne. 1 )  STOP 1
    IF (A25 .Ne. 1 )  STOP 1
    IF (A26 .Ne. 1 )  STOP 1
    IF (A27 .Ne. 1 )  STOP 1
    IF (A28 .Ne. 1 )  STOP 1
    IF (A29 .Ne. 1 )  STOP 1

    IF (A30 .Ne. 1 ) STOP 1
    IF (A31 .Ne. 1 ) STOP 1
    IF (A32 .Ne. 1 ) STOP 1
    IF (A33 .Ne. 1 ) STOP 1
    IF (A34 .Ne. 1 ) STOP 1
    IF (A35 .Ne. 1 ) STOP 1
    IF (A36 .Ne. 1 ) STOP 1
    IF (A37 .Ne. 1 ) STOP 1
    IF (A38 .Ne. 1 ) STOP 1
    IF (A39 .Ne. 1 ) STOP 1

    IF (A40 .Ne. 1 ) STOP 1
    IF (A41 .Ne. 1 ) STOP 1
    IF (A42 .Ne. 1 ) STOP 1
    IF (A43 .Ne. 1 ) STOP 1
    IF (A44 .Ne. 1 ) STOP 1
    IF (A45 .Ne. 1 ) STOP 1
    IF (A46 .Ne. 1 ) STOP 1
    IF (A47 .Ne. 1 ) STOP 1
    IF (A48 .Ne. 1 ) STOP 1
    IF (A49 .Ne. 1 ) STOP 1

    IF (A50 .Ne. 1 ) STOP 1
    IF (A51 .Ne. 1 ) STOP 1
    IF (A52 .Ne. 1 ) STOP 1
    IF (A53 .Ne. 1 ) STOP 1
    IF (A54 .Ne. 1 ) STOP 1
    IF (A55 .Ne. 1 ) STOP 1
    IF (A56 .Ne. 1 ) STOP 1
    IF (A57 .Ne. 1 ) STOP 1
    IF (A58 .Ne. 1 ) STOP 1
    IF (A59 .Ne. 1 ) STOP 1

    IF (A60 .Ne. 1 ) STOP 1
    IF (A61 .Ne. 1 ) STOP 1
    IF (A62 .Ne. 1 ) STOP 1
    IF (A63 .Ne. 1 ) STOP 1
    IF (A64 .Ne. 1 ) STOP 1
    IF (A65 .Ne. 1 ) STOP 1
    IF (A66 .Ne. 1 ) STOP 1
    IF (A67 .Ne. 1 ) STOP 1
    IF (A68 .Ne. 1 ) STOP 1
    IF (A69 .Ne. 1 ) STOP 1

    IF (A70 .Ne. 1 ) STOP 1
    IF (A71 .Ne. 1 ) STOP 1
    IF (A72 .Ne. 1 ) STOP 1
    IF (A73 .Ne. 1 ) STOP 1
    IF (A74 .Ne. 1 ) STOP 1
    IF (A75 .Ne. 1 ) STOP 1
    IF (A76 .Ne. 1 ) STOP 1
    IF (A77 .Ne. 1 ) STOP 1
    IF (A78 .Ne. 1 ) STOP 1
    IF (A79 .Ne. 1 ) STOP 1

    IF (A80 .Ne. 1 ) STOP 1
    IF (A81 .Ne. 1 ) STOP 1
    IF (A82 .Ne. 1 ) STOP 1
    IF (A83 .Ne. 1 ) STOP 1
    IF (A84 .Ne. 1 ) STOP 1
    IF (A85 .Ne. 1 ) STOP 1
    IF (A86 .Ne. 1 ) STOP 1
    IF (A87 .Ne. 1 ) STOP 1
    IF (A88 .Ne. 1 ) STOP 1
    IF (A89 .Ne. 1 ) STOP 1

    IF (A90 .Ne. 1 ) STOP 1
    IF (A91 .Ne. 1 ) STOP 1
    IF (A92 .Ne. 1 ) STOP 1
    IF (A93 .Ne. 1 ) STOP 1
    IF (A94 .Ne. 1 ) STOP 1
    IF (A95 .Ne. 1 ) STOP 1
    IF (A96 .Ne. 1 ) STOP 1
    IF (A97 .Ne. 1 ) STOP 1
    IF (A98 .Ne. 1 ) STOP 1
    IF (A99 .Ne. 1 ) STOP 1

  END ASSOCIATE

  END


