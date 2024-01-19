!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April 15, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : nint()
!*				 ieee_invalid
!*
!*  REFERENCE                  : Feature 180920
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This testcase verifies that when an
!*				 invalid argument is passed to the
!*				 NINT function, ieee_invalid flag
!*				 is set to true.
!*				 The testcase also verifies that the other
!*				 flags remain clear and false.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

	program ieeemisc21
	  use ieee_arithmetic
	  implicit none

	  logical*4 flag_values(5)

          integer                 int0,int00,int000,int0000,int00000
          integer*1               int1, int11,int111
          integer*2               int2, int22,int222
          integer*4               int4, int44,int444
          integer*8               int8, int88,int888

!*  Positive Infinities
       	  real(4), parameter :: PINF_4 = z"7f800000"
          real(8), parameter :: PINF_8 = z"7ff0000000000000"
          real(16), parameter :: PINF_16 = z"7ff00000000000000000000000000000"

!***********************************************************************
!*  Note:
!*    Exception flags:
!*       flag_values(1) = IEEE_OVERFLOW flag
!*       flag_values(2) = IEEE_DIVIDE_BY_ZERO flag
!*       flag_values(3) = IEEE_INVALID flag
!*       flag_values(4) = IEEE_UNDERFLOW flag
!*       flag_values(5) = IEEE_INEXACT flag
!*
!***********************************************************************

!***********************************************************************
!*  Check to see that positive infinity will cause IEEE invalid exception
!*  flag to signal. No other flags should be triggered.
!***********************************************************************
!*  default integer and real(4) positive infinity
	  int0 = nint(PINF_4)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)        	error stop 1
          if (flag_values(2) .neqv. .false.)            error stop 2
          if (flag_values(3) .neqv. .true.)             error stop 3
          if (flag_values(4) .neqv. .false.)            error stop 4
          if (flag_values(5) .neqv. .false.)            error stop 5

	  call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))      	error stop 6

!* --------------------------------------------------------------------
!*  integer(1) and real(4) positive infinity
	  int1 = nint(PINF_4)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 7
          if (flag_values(2) .neqv. .false.)            error stop 8
          if (flag_values(3) .neqv. .true.)             error stop 9
          if (flag_values(4) .neqv. .false.)            error stop 10
          if (flag_values(5) .neqv. .false.)            error stop 11

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 12

!* --------------------------------------------------------------------
!*  integer(2) and real(4) positive infinity
          int2 = nint(PINF_4)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 13
          if (flag_values(2) .neqv. .false.)            error stop 14
          if (flag_values(3) .neqv. .true.)             error stop 15
          if (flag_values(4) .neqv. .false.)            error stop 16
          if (flag_values(5) .neqv. .false.)            error stop 17

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 18

!* --------------------------------------------------------------------
!*  integer(4) and real(4) positive infinity
          int4 = nint(PINF_4)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 19
          if (flag_values(2) .neqv. .false.)            error stop 20
          if (flag_values(3) .neqv. .true.)             error stop 21
          if (flag_values(4) .neqv. .false.)            error stop 22
          if (flag_values(5) .neqv. .false.)            error stop 23

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 24

!* --------------------------------------------------------------------
!*  integer(8) and real(4) positive infinity
          int8 = nint(PINF_4)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 25
          if (flag_values(2) .neqv. .false.)            error stop 26
          if (flag_values(3) .neqv. .true.)             error stop 27
          if (flag_values(4) .neqv. .false.)            error stop 28
          if (flag_values(5) .neqv. .false.)            error stop 29

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 30

!* --------------------------------------------------------------------
!*  default integer and real(8) positive infinity
          int00 = nint(PINF_8)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 31
          if (flag_values(2) .neqv. .false.)            error stop 32
          if (flag_values(3) .neqv. .true.)             error stop 33
          if (flag_values(4) .neqv. .false.)            error stop 34
          if (flag_values(5) .neqv. .false.)            error stop 35

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 36

!* --------------------------------------------------------------------
!*  integer(1) and real(8) positive infinity
          int11 = nint(PINF_8)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 37
          if (flag_values(2) .neqv. .false.)            error stop 38
          if (flag_values(3) .neqv. .true.)             error stop 39
          if (flag_values(4) .neqv. .false.)            error stop 40
          if (flag_values(5) .neqv. .false.)            error stop 41

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 42

!* --------------------------------------------------------------------
!*  integer(2) and real(8) positive infinity
          int22 = nint(PINF_8)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 43
          if (flag_values(2) .neqv. .false.)            error stop 44
          if (flag_values(3) .neqv. .true.)             error stop 45
          if (flag_values(4) .neqv. .false.)            error stop 46
          if (flag_values(5) .neqv. .false.)            error stop 47

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 48

!* --------------------------------------------------------------------
!*  integer(4) and real(8) positive infinity
          int44 = nint(PINF_8)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 49
          if (flag_values(2) .neqv. .false.)            error stop 50
          if (flag_values(3) .neqv. .true.)             error stop 51
          if (flag_values(4) .neqv. .false.)            error stop 52
          if (flag_values(5) .neqv. .false.)            error stop 53

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 54

!* --------------------------------------------------------------------
!*  integer(8) and real(8) positive infinity
          int88 = nint(PINF_8)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 55
          if (flag_values(2) .neqv. .false.)            error stop 56
          if (flag_values(3) .neqv. .true.)             error stop 57
          if (flag_values(4) .neqv. .false.)            error stop 58
          if (flag_values(5) .neqv. .false.)            error stop 59

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 60

!* --------------------------------------------------------------------
!*  default integer and real(16) positive infinity
          int000 = nint(PINF_16)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 61
          if (flag_values(2) .neqv. .false.)            error stop 62
          if (flag_values(3) .neqv. .true.)             error stop 63
          if (flag_values(4) .neqv. .false.)            error stop 64
          if (flag_values(5) .neqv. .false.)            error stop 65

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 66

!* --------------------------------------------------------------------
!*  integer(1) and real(16) positive infinity
          int111 = nint(PINF_16)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 67
          if (flag_values(2) .neqv. .false.)            error stop 68
          if (flag_values(3) .neqv. .true.)             error stop 69
          if (flag_values(4) .neqv. .false.)            error stop 70
          if (flag_values(5) .neqv. .false.)            error stop 71

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 72

!* --------------------------------------------------------------------
!*  integer(2) and real(16) positive infinity
          int222 = nint(PINF_16)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 73
          if (flag_values(2) .neqv. .false.)            error stop 74
          if (flag_values(3) .neqv. .true.)             error stop 75
          if (flag_values(4) .neqv. .false.)            error stop 76
          if (flag_values(5) .neqv. .false.)            error stop 77

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 78

!* --------------------------------------------------------------------
!*  integer(4) and real(16) positive infinity
          int444 = nint(PINF_16)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 79
          if (flag_values(2) .neqv. .false.)            error stop 80
          if (flag_values(3) .neqv. .true.)             error stop 81
          if (flag_values(4) .neqv. .false.)            error stop 82
          if (flag_values(5) .neqv. .false.)            error stop 83

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 84

!* --------------------------------------------------------------------
!*  integer(8) and real(16) positive infinity
          int888 = nint(PINF_16)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 85
          if (flag_values(2) .neqv. .false.)            error stop 86
          if (flag_values(3) .neqv. .true.)             error stop 87
          if (flag_values(4) .neqv. .false.)            error stop 88
          if (flag_values(5) .neqv. .false.)            error stop 89

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 90

!* --------------------------------------------------------------------
!*  iqnint: default integer and real(16) positive infinity
          int0000 = iqnint(PINF_16)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 91
          if (flag_values(2) .neqv. .false.)            error stop 92
          if (flag_values(3) .neqv. .true.)             error stop 93
          if (flag_values(4) .neqv. .false.)            error stop 94
          if (flag_values(5) .neqv. .false.)            error stop 95

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 96

!* --------------------------------------------------------------------
!*  idnint: default integer and real(8) positive infinity
          int00000 = idnint(PINF_8)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 97
          if (flag_values(2) .neqv. .false.)            error stop 98
          if (flag_values(3) .neqv. .true.)             error stop 99
          if (flag_values(4) .neqv. .false.)            error stop 100
          if (flag_values(5) .neqv. .false.)            error stop 101

          call ieee_set_flag(ieee_invalid, .false.)
          call ieee_get_flag(ieee_all, flag_values)
          if (any(flag_values .neqv. .false.))          error stop 102

!* --------------------------------------------------------------------


        end program
!=======================================================================

