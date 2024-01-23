! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with integer array.
!*   - FORTRAN code only , the interoperable function is implemented
!*     in Fortran and called in Fortran.
!*   - passing 1-dim  derived type  array arguments
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE Personal_details
  IMPLICIT NONE
  TYPE Person
     REAL:: Weight
     INTEGER :: Age
     CHARACTER :: Sex
  END TYPE Person
END MODULE Personal_details

PROGRAM Survey
  use assertmod
  USE Personal_details
  IMPLICIT NONE
  INTEGER ,PARAMETER:: Max_no=100
  logical :: precision_R4,test
  TYPE (Person), DIMENSION(1:Max_no) :: Patient
  INTEGER :: I,No_of_patients
  REAL :: Male_average, Female_average ,result
  INTERFACE

     SUBROUTINE Read_data(Data,No)
       USE Personal_details
       IMPLICIT NONE
       TYPE (Person), DIMENSION (:), INTENT(INOUT):: Data
       INTEGER, INTENT(INOUT):: No

     END SUBROUTINE Read_Data

     FUNCTION Stats(Data,No,M_a,F_a)
       USE Personal_details
       IMPLICIT NONE
       TYPE(Person), DIMENSION (:) :: Data
       REAL:: M_a,F_a,Stats
       INTEGER :: No
     END FUNCTION Stats

  END INTERFACE
  !
  No_of_patients =4
  CALL Read_data(Patient,No_of_patients)
  result  =  Stats (Patient , No_of_patients , &
       Male_average , Female_average)
  PRINT*, 'The result  is ', result
  test = precision_R4(result, 60.)
  call assert(test,'The result is not correct',9)
  PRINT*, 'Average male weight is ',Male_average
  PRINT*, 'Average female weight is ',Female_average
END PROGRAM Survey

SUBROUTINE Read_Data(Data,No)
  USE Personal_details
  IMPLICIT NONE
  TYPE (PERSON), DIMENSION (:), INTENT(INOUT)::Data
  INTEGER, INTENT(INOUT):: No

  INTEGER :: I
  DO I=1,No,2
     Data(I)%Weight = 50. + 5*I
     Data(I)%Age = 10 + 5 *I
     Data(I)%Sex ='F'
  END DO

  DO I=2,No,2
     Data(I)%Weight = 60. + 5*I
     Data(I)%Age = 12 + 5 *I
     Data(I)%Sex ='M'
  END DO

END SUBROUTINE Read_Data

Function Stats(Data,No,M_a,F_a)
  USE Personal_details
  IMPLICIT NONE
  REAL :: Stats
  TYPE(Person), DIMENSION(:)::Data
  REAL :: M_a,F_a
  INTEGER:: No
  INTEGER :: I,No_f,No_m
  M_a=0.0; F_a=0.0;No_f=0; No_m =0
  DO I=1,No
     IF ( Data(I)%Sex == 'M' &
          .OR. Data(I)%Sex == 'm') THEN
        M_a=M_a+Data(I)%Weight
        No_m=No_m+1
     ELSEIF(Data(I)%Sex == 'F' &
          .OR. Data(I)%Sex == 'f') THEN
        F_a=F_a +Data(I)%Weight
        No_f=No_f+1
     ENDIF
  END DO
  IF (No_m > 0 ) THEN
     M_a = M_a/No_m
  ENDIF
  IF (No_f > 0 ) THEN
     F_a = F_a/No_f
  ENDIF
  Stats = F_a
END Function  Stats
