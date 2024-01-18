! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: EvalFirst.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : EvalFirst
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 04, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    
!*  The selector is evaluated prior to execution of the associate block 
!*   
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM EvalFirst
  IMPLICIT NONE
  LOGICAL(8) :: LArr(0:127)=.FALSE. 

  ASSOCIATE (             &
  & A1   => FUN(LArr,1),  &
  & A2   => FUN(LArr,2),  &
  & A3   => FUN(LArr,3),  &
  & A4   => FUN(LArr,4),  &
  & A5   => FUN(LArr,5),  &
  & A6   => FUN(LArr,6),  &
  & A7   => FUN(LArr,7),  &
  & A8   => FUN(LArr,8),  &
  & A9   => FUN(LArr,9),  &
  & A0   => FUN(LArr,0),  &
  &                         &
  & A10   => FUN(LArr,10),  &
  & A11   => FUN(LArr,11),  &
  & A12   => FUN(LArr,12),  &
  & A13   => FUN(LArr,13),  &
  & A14   => FUN(LArr,14),  &
  & A15   => FUN(LArr,15),  &
  & A16   => FUN(LArr,16),  &
  & A17   => FUN(LArr,17),  &
  & A18   => FUN(LArr,18),  &
  & A19   => FUN(LArr,19),  &
  &                         &
  & A20   => FUN(LArr,20),  &
  & A21   => FUN(LArr,21),  &
  & A22   => FUN(LArr,22),  &
  & A23   => FUN(LArr,23),  &
  & A24   => FUN(LArr,24),  &
  & A25   => FUN(LArr,25),  &
  & A26   => FUN(LArr,26),  &
  & A27   => FUN(LArr,27),  &
  & A28   => FUN(LArr,28),  &
  & A29   => FUN(LArr,29),  &
  &                         &
  & A30   => FUN(LArr,30),  &
  & A31   => FUN(LArr,31),  &
  & A32   => FUN(LArr,32),  &
  & A33   => FUN(LArr,33),  &
  & A34   => FUN(LArr,34),  &
  & A35   => FUN(LArr,35),  &
  & A36   => FUN(LArr,36),  &
  & A37   => FUN(LArr,37),  &
  & A38   => FUN(LArr,38),  &
  & A39   => FUN(LArr,39),  &
  &                         &
  & A40   => FUN(LArr,40),  &
  & A41   => FUN(LArr,41),  &
  & A42   => FUN(LArr,42),  &
  & A43   => FUN(LArr,43),  &
  & A44   => FUN(LArr,44),  &
  & A45   => FUN(LArr,45),  &
  & A46   => FUN(LArr,46),  &
  & A47   => FUN(LArr,47),  &
  & A48   => FUN(LArr,48),  &
  & A49   => FUN(LArr,49),  &
  &                         &
  & A50   => FUN(LArr,50),  &
  & A51   => FUN(LArr,51),  &
  & A52   => FUN(LArr,52),  &
  & A53   => FUN(LArr,53),  &
  & A54   => FUN(LArr,54),  &
  & A55   => FUN(LArr,55),  &
  & A56   => FUN(LArr,56),  &
  & A57   => FUN(LArr,57),  &
  & A58   => FUN(LArr,58),  &
  & A59   => FUN(LArr,59),  &
  &                         &
  & A60   => FUN(LArr,60),  &
  & A61   => FUN(LArr,61),  &
  & A62   => FUN(LArr,62),  &
  & A63   => FUN(LArr,63),  &
  & A64   => FUN(LArr,64),  &
  & A65   => FUN(LArr,65),  &
  & A66   => FUN(LArr,66),  &
  & A67   => FUN(LArr,67),  &
  & A68   => FUN(LArr,68),  &
  & A69   => FUN(LArr,69),  &
  &                         &
  & A70   => FUN(LArr,70),  &
  & A71   => FUN(LArr,71),  &
  & A72   => FUN(LArr,72),  &
  & A73   => FUN(LArr,73),  &
  & A74   => FUN(LArr,74),  &
  & A75   => FUN(LArr,75),  &
  & A76   => FUN(LArr,76),  &
  & A77   => FUN(LArr,77),  &
  & A78   => FUN(LArr,78),  &
  & A79   => FUN(LArr,79),  &
  &                         &
  & A80   => FUN(LArr,80),  &
  & A81   => FUN(LArr,81),  &
  & A82   => FUN(LArr,82),  &
  & A83   => FUN(LArr,83),  &
  & A84   => FUN(LArr,84),  &
  & A85   => FUN(LArr,85),  &
  & A86   => FUN(LArr,86),  &
  & A87   => FUN(LArr,87),  &
  & A88   => FUN(LArr,88),  &
  & A89   => FUN(LArr,89),  &
  &                         &
  & A90   => FUN(LArr,90),  &
  & A91   => FUN(LArr,91),  &
  & A92   => FUN(LArr,92),  &
  & A93   => FUN(LArr,93),  &
  & A94   => FUN(LArr,94),  &
  & A95   => FUN(LArr,95),  &
  & A96   => FUN(LArr,96),  &
  & A97   => FUN(LArr,97),  &
  & A98   => FUN(LArr,98),  &
  & A99   => FUN(LArr,99),  &
  &                           &
  & A100   => FUN(LArr,100),  &
  & A101   => FUN(LArr,101),  &
  & A102   => FUN(LArr,102),  &
  & A103   => FUN(LArr,103),  &
  & A104   => FUN(LArr,104),  &
  & A105   => FUN(LArr,105),  &
  & A106   => FUN(LArr,106),  &
  & A107   => FUN(LArr,107),  &
  & A108   => FUN(LArr,108),  &
  & A109   => FUN(LArr,109),  &
  &                           &
  & A110   => FUN(LArr,110),  &
  & A111   => FUN(LArr,111),  &
  & A112   => FUN(LArr,112),  &
  & A113   => FUN(LArr,113),  &
  & A114   => FUN(LArr,114),  &
  & A115   => FUN(LArr,115),  &
  & A116   => FUN(LArr,116),  &
  & A117   => FUN(LArr,117),  &
  & A118   => FUN(LArr,118),  &
  & A119   => FUN(LArr,119),  &
  &                           &
  & A120   => FUN(LArr,120),  &
  & A121   => FUN(LArr,121),  &
  & A122   => FUN(LArr,122),  &
  & A123   => FUN(LArr,123),  &
  & A124   => FUN(LArr,124),  &
  & A125   => FUN(LArr,125),  &
  & A126   => FUN(LArr,126),  &
  & A127   => FUN(LArr,127)    )

    IF (( A1 .OR. A2 .OR. A3 .OR. A4 .OR. A5 .OR. &
    &     A6 .OR. A7 .OR. A8 .OR. A9 .OR. A0 ) .EQV. .TRUE. ) STOP 10

    IF (( A10 .OR. A11 .OR. A12 .OR. A13 .OR. A14 .OR. &
    &     A15 .OR. A16 .OR. A17 .OR. A18 .OR. A19 ) .EQV. .TRUE. ) STOP 11

    IF (( A20 .OR. A21 .OR. A22 .OR. A23 .OR. A24 .OR. &
    &     A25 .OR. A26 .OR. A27 .OR. A28 .OR. A29 ) .EQV. .TRUE. ) STOP 12

    IF (( A30 .OR. A31 .OR. A32 .OR. A33 .OR. A34 .OR. &
    &     A35 .OR. A36 .OR. A37 .OR. A38 .OR. A39 ) .EQV. .TRUE. ) STOP 13

    IF (( A40 .OR. A41 .OR. A42 .OR. A43 .OR. A44 .OR. &
    &     A45 .OR. A46 .OR. A47 .OR. A48 .OR. A49 ) .EQV. .TRUE. ) STOP 14

    IF (( A50 .OR. A51 .OR. A52 .OR. A53 .OR. A54 .OR. &
    &     A55 .OR. A56 .OR. A57 .OR. A58 .OR. A59 ) .EQV. .TRUE. ) STOP 15

    IF (( A60 .OR. A61 .OR. A62 .OR. A63 .OR. A64 .OR. &
    &     A65 .OR. A66 .OR. A67 .OR. A68 .OR. A69 ) .EQV. .TRUE. ) STOP 16

    IF (( A70 .OR. A71 .OR. A72 .OR. A73 .OR. A74 .OR. &
    &     A75 .OR. A76 .OR. A77 .OR. A78 .OR. A79 ) .EQV. .TRUE. ) STOP 17

    IF (( A80 .OR. A81 .OR. A82 .OR. A83 .OR. A84 .OR. &
    &     A85 .OR. A81 .OR. A87 .OR. A88 .OR. A89 ) .EQV. .TRUE. ) STOP 18

    IF (( A90 .OR. A91 .OR. A92 .OR. A93 .OR. A94 .OR. &
    &     A95 .OR. A96 .OR. A97 .OR. A98 .OR. A99 ) .EQV. .TRUE. ) STOP 19

    IF (( A100 .OR. A101 .OR. A102 .OR. A103 .OR. A104 .OR. &
    &     A105 .OR. A106 .OR. A107 .OR. A108 .OR. A109 ) .EQV. .TRUE. ) STOP 20

    IF (( A110 .OR. A111 .OR. A112 .OR. A113 .OR. A114 .OR. &
    &     A115 .OR. A116 .OR. A117 .OR. A118 .OR. A119 ) .EQV. .TRUE. ) STOP 21

    IF (( A120 .OR. A121 .OR. A122 .OR. A123 .OR. A124 .OR. &
    &     A125 .OR. A126 .OR. A127  )                    .EQV. .TRUE. ) STOP 22

    IF (ANY( LArr .NEQV. .TRUE. )) STOP  33
 
  END ASSOCIATE

  CONTAINS

  FUNCTION Fun(LArr,Arg)
  INTEGER :: Arg
  LOGICAL(8) Fun, LArr(0:)
    Fun       = LArr(Arg)
    LArr(Arg) = .NOT. LArr(Arg)
  END FUNCTION

  END
