!***********************************************************************
!*
!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : SIGN=specifier in list-directed output
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 : SIGN
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  DESCRIPTION                : Testing the functionality of the
!*                               SIGN= specifier used in list-directed output
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
  implicit none
  real, dimension(10)     :: r
  integer, dimension(10)  :: m
  integer i, j

  do i = 1, 10
     m(i) = i * i
     r(i) = sqrt(1.0 * m(i))
  end do

  open (1, file="sign11.out")

  !* The sign mode of nameslist output is set to plus *!
  write(1, *, sign="plus") r, m

  !* The sign mode of nameslist output is set to suppress *!
  write(1, *, sign="suppress") r, m

  !* The sign mode of nameslist output is set to processor_defined *!
  !*  which should act the same as suppress *!
  write(1, *, sign="processor_defined") r, m
  close(1)

  !* The sign mode in OPEN is set to plus which will be overwrite by
  !* WRITE statement for namelist output
  open (1, file="sign11.out", sign="plus", status="old", position="append")
  write(1, *, sign="suppress") r, m
  close(1)

  !* The sign mode in OPEN is set to suppress which will be overwrite by
  !* WRITE statement for namelist output
  open (1, file="sign11.out", sign="suppress", status="old", position="append")
  write(1, *, sign="plus") r, m
  close(1)

  end
