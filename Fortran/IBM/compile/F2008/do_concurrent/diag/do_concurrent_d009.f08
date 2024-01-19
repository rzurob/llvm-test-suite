!*******************************************************************************
!*
!============================================================================
!*
!============================================================================
!*
!*  DATE                       : 2015-03-20
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION                :
!*   - A reference to IEEE_GET_FLAG, IEEE_SET_HALTING_MODE, IEEE_GET_HALTING_MODE
!*     from IEEE_EXCEPTIONS appears in a do concurrent construct
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
      program main
        use IEEE_EXCEPTIONS
        logical overflow_flag
        integer i, j

        DO CONCURRENT (j = 0:100, i = 0:100)
            tmp = IEEE_GET_HALTING_MODE(IEEE_USUAL,halt)
        END DO

        DO CONCURRENT (j = 0:100, i = 0:100)
            call IEEE_SET_HALTING_MODE(IEEE_USUAL,.true.)
        END DO

        DO CONCURRENT (j = 0:100, i = 0:100)
            call IEEE_GET_FLAG(IEEE_OVERFLOW,overflow_flag)
        END DO

      end
