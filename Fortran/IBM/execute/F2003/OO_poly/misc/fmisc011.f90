! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 292700)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc011
    character (20), parameter :: c1 = 'xlftest team'
    character (15) :: c2 = 'xlftest team'

    associate (x => c1)
        if (x(1:7) /= 'xlftest') error stop 1_4

        if (x(4:7) /= 'test') error stop 2_4

        associate (x1 => x(4:7))

            if (x1 /= 'test') error stop 3_4
        end associate
    end associate


    associate (x => c2(2:10))

        if (x(1:7) /= 'lftest ') error stop 4_4
        if (x(4:7) /= 'est ') error stop 5_4
    end associate
end

